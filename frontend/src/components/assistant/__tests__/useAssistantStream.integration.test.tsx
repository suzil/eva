import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { renderHook, act, render, screen, fireEvent } from '@testing-library/react'
import React from 'react'
import { useAssistantStream } from '../../../hooks/useAssistantStream'
import { useUiStore } from '../../../store/uiStore'
import { useCanvasStore } from '../../../store/canvasStore'
import { GraphProposalCard } from '../GraphProposalCard'
import type { Graph, Program } from '../../../types'

// ---------------------------------------------------------------------------
// Mock WebSocket
// ---------------------------------------------------------------------------

class MockWebSocket {
  static instances: MockWebSocket[] = []
  readyState = 1 // WebSocket.OPEN
  send = vi.fn()
  close = vi.fn()
  onopen: (() => void) | null = null
  onmessage: ((e: { data: string }) => void) | null = null
  onerror: (() => void) | null = null
  onclose: (() => void) | null = null
  constructor(_url: string) {
    MockWebSocket.instances.push(this)
  }
  simulateMessage(data: object) {
    this.onmessage?.({ data: JSON.stringify(data) })
  }
  simulateOpen() {
    this.onopen?.()
  }
}

vi.stubGlobal('WebSocket', MockWebSocket)

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const PROGRAM_ID = 'prog-test'

const MOCK_PROGRAMS: Program[] = [
  { id: PROGRAM_ID, name: 'Test Program', state: 'draft', createdAt: '', updatedAt: '' },
]

/** Minimal valid graph — single Trigger node satisfies validateProposedGraph. */
const MOCK_GRAPH: Graph = {
  nodes: {
    'n-trigger': {
      id: 'n-trigger',
      programId: PROGRAM_ID,
      label: 'Start',
      type: {
        type: 'trigger',
        config: { triggerType: 'manual' },
      },
      posX: 0,
      posY: 0,
    },
  },
  edges: [],
}

function getLatestWs(): MockWebSocket {
  return MockWebSocket.instances[MockWebSocket.instances.length - 1]
}

beforeEach(() => {
  MockWebSocket.instances = []
  vi.clearAllMocks()
  useUiStore.setState({
    assistantConversations: {},
    mode: 'author',
    activeRunId: null,
    logEntries: [],
  })
  useCanvasStore.setState({
    nodes: [],
    edges: [],
    currentProgramId: PROGRAM_ID,
    selectedNodeId: null,
    selectedEdgeId: null,
    isDirty: false,
    nodeStepStates: {},
    nodeStepErrors: {},
    past: [],
    future: [],
    triggerFitView: false,
    previewOverlayGraph: null,
    hoveredNodeId: null,
  })
})

afterEach(() => {
  MockWebSocket.instances = []
})

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('useAssistantStream', () => {
  it('accumulates assistant_token events into streamingText', async () => {
    const { result } = renderHook(() => useAssistantStream(PROGRAM_ID, MOCK_PROGRAMS))
    const ws = getLatestWs()
    ws.simulateOpen()

    act(() => {
      ws.simulateMessage({ type: 'assistant_token', conversationId: PROGRAM_ID, token: 'Ana', timestamp: '' })
      ws.simulateMessage({ type: 'assistant_token', conversationId: PROGRAM_ID, token: 'lysis', timestamp: '' })
    })

    expect(result.current.streamingText).toBe('Analysis')
  })

  it('assistant_reply with text message appends to store and clears streamingText', async () => {
    const { result } = renderHook(() => useAssistantStream(PROGRAM_ID, MOCK_PROGRAMS))
    const ws = getLatestWs()
    ws.simulateOpen()

    act(() => {
      ws.simulateMessage({ type: 'assistant_token', conversationId: PROGRAM_ID, token: 'Hello', timestamp: '' })
    })
    expect(result.current.streamingText).toBe('Hello')

    act(() => {
      ws.simulateMessage({
        type: 'assistant_reply',
        conversationId: PROGRAM_ID,
        message: { type: 'text', text: 'Analysis complete.', timestamp: Date.now() },
        timestamp: '',
      })
    })

    expect(result.current.streamingText).toBe('')
    const thread = useUiStore.getState().assistantConversations[PROGRAM_ID]
    expect(thread).toBeDefined()
    expect(thread.messages).toHaveLength(1)
    expect(thread.messages[0]).toMatchObject({ type: 'text', text: 'Analysis complete.' })
  })

  it('assistant_reply with graph_proposal message appends proposal to store', async () => {
    const { result: _result } = renderHook(() => useAssistantStream(PROGRAM_ID, MOCK_PROGRAMS))
    const ws = getLatestWs()
    ws.simulateOpen()

    act(() => {
      ws.simulateMessage({
        type: 'assistant_reply',
        conversationId: PROGRAM_ID,
        message: {
          type: 'graph_proposal',
          graph: MOCK_GRAPH,
          summary: 'A single-trigger program.',
          timestamp: Date.now(),
        },
        timestamp: '',
      })
    })

    const thread = useUiStore.getState().assistantConversations[PROGRAM_ID]
    expect(thread.messages).toHaveLength(1)
    const msg = thread.messages[0]
    expect(msg.type).toBe('graph_proposal')
    if (msg.type === 'graph_proposal') {
      expect(msg.summary).toBe('A single-trigger program.')
      expect(Object.keys(msg.graph.nodes)).toHaveLength(1)
    }
  })
})

describe('conversation eviction', () => {
  it('keeps at most 50 messages, evicting oldest when limit is exceeded', () => {
    const { appendAssistantMessage } = useUiStore.getState()

    // Append 51 messages
    for (let i = 0; i < 51; i++) {
      act(() => {
        appendAssistantMessage(PROGRAM_ID, { type: 'text', text: `Message ${i}`, timestamp: i })
      })
    }

    const thread = useUiStore.getState().assistantConversations[PROGRAM_ID]
    expect(thread.messages).toHaveLength(50)
    // Oldest (message 0) should be gone; message 1 is now first
    expect(thread.messages[0]).toMatchObject({ type: 'text', text: 'Message 1' })
    // Most recent should be message 50
    expect(thread.messages[49]).toMatchObject({ type: 'text', text: 'Message 50' })
  })
})

describe('GraphProposalCard', () => {
  it('Preview button sets previewOverlayGraph in canvasStore', () => {
    render(<GraphProposalCard graph={MOCK_GRAPH} summary="A trigger node." />)
    fireEvent.click(screen.getByRole('button', { name: /preview on canvas/i }))
    expect(useCanvasStore.getState().previewOverlayGraph).toEqual(MOCK_GRAPH)
  })

  it('Accept button loads the graph into canvasStore and marks dirty', () => {
    render(<GraphProposalCard graph={MOCK_GRAPH} summary="A trigger node." />)
    fireEvent.click(screen.getByRole('button', { name: /accept/i }))

    const state = useCanvasStore.getState()
    expect(state.isDirty).toBe(true)
    expect(state.nodes).toHaveLength(1)
    expect(state.nodes[0].id).toBe('n-trigger')
  })

  it('Accept button shows accepted state after load', () => {
    render(<GraphProposalCard graph={MOCK_GRAPH} summary="A trigger node." />)
    fireEvent.click(screen.getByRole('button', { name: /accept/i }))
    expect(screen.getByText(/graph accepted/i)).toBeInTheDocument()
  })
})
