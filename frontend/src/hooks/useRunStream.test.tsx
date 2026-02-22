import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { renderHook } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import React from 'react'
import { useRunStream } from './useRunStream'
import { useCanvasStore } from '../store/canvasStore'
import { useUiStore } from '../store/uiStore'

// ---------------------------------------------------------------------------
// Mock fetchRunDetail
// ---------------------------------------------------------------------------

vi.mock('../api/client', () => ({
  fetchRunDetail: vi.fn().mockResolvedValue({
    id: 'run-1',
    programId: 'prog-1',
    state: 'completed',
    steps: [
      {
        id: 's1',
        runId: 'run-1',
        nodeId: 'n1',
        state: 'completed',
        output: { type: 'agent_output', payload: 'Hello world' },
        error: null,
        startedAt: null,
        completedAt: null,
      },
    ],
    startedAt: null,
    completedAt: null,
  }),
}))

// ---------------------------------------------------------------------------
// Mock WebSocket
// ---------------------------------------------------------------------------

class MockWebSocket {
  static instances: MockWebSocket[] = []
  send = vi.fn()
  close = vi.fn()
  onopen: (() => void) | null = null
  onmessage: ((e: { data: string }) => void) | null = null
  onerror: (() => void) | null = null
  onclose: (() => void) | null = null
  constructor(_url: string) {
    MockWebSocket.instances.push(this)
  }
  /** Simulate the server sending a message. */
  simulateMessage(data: object) {
    this.onmessage?.({ data: JSON.stringify(data) })
  }
  /** Simulate the connection opening. */
  simulateOpen() {
    this.onopen?.()
  }
}

vi.stubGlobal('WebSocket', MockWebSocket)

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function wrapper({ children }: { children: React.ReactNode }) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } })
  return <QueryClientProvider client={client}>{children}</QueryClientProvider>
}

function getLatestWs(): MockWebSocket {
  return MockWebSocket.instances[MockWebSocket.instances.length - 1]
}

beforeEach(() => {
  MockWebSocket.instances = []
  vi.clearAllMocks()
  useCanvasStore.setState({
    nodes: [{ id: 'n1', type: 'agent', position: { x: 0, y: 0 }, data: { label: 'A', nodeType: { type: 'agent', config: { provider: 'openai', model: 'gpt-4o', systemPrompt: '', temperature: 0.7, maxIterations: 5, responseFormat: 'text' } } } }],
    edges: [],
    nodeStepStates: {},
    nodeStepErrors: {},
    past: [],
    future: [],
    isDirty: false,
    currentProgramId: null,
    selectedNodeId: null,
    selectedEdgeId: null,
    triggerFitView: false,
  })
  useUiStore.setState({
    activeRunId: null,
    inspectedRunId: null,
    llmOutput: '',
    logEntries: [],
    runError: null,
    activeBottomTab: 'logs',
    bottomPanelOpen: false,
  })
})

afterEach(() => {
  MockWebSocket.instances = []
})

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('useRunStream', () => {
  it('does not create a WebSocket when runId is null', () => {
    renderHook(() => useRunStream(null, 'prog-1'), { wrapper })
    expect(MockWebSocket.instances).toHaveLength(0)
  })

  it('creates a WebSocket and sends subscribe message on open', () => {
    renderHook(() => useRunStream('run-1', 'prog-1'), { wrapper })
    const ws = getLatestWs()
    expect(ws).toBeDefined()
    ws.simulateOpen()
    expect(ws.send).toHaveBeenCalledWith(
      JSON.stringify({ action: 'subscribe', topic: 'run:run-1' }),
    )
  })

  it('step_state event updates nodeStepState via canvasStore', () => {
    renderHook(() => useRunStream('run-1', 'prog-1'), { wrapper })
    const ws = getLatestWs()
    ws.simulateOpen()
    ws.simulateMessage({ type: 'step_state', nodeId: 'n1', state: 'running' })
    expect(useCanvasStore.getState().nodeStepStates['n1']).toBe('running')
  })

  it('llm_token event appends token to llmOutput', () => {
    renderHook(() => useRunStream('run-1', 'prog-1'), { wrapper })
    const ws = getLatestWs()
    ws.simulateMessage({ type: 'llm_token', token: 'Hello' })
    ws.simulateMessage({ type: 'llm_token', token: ' world' })
    expect(useUiStore.getState().llmOutput).toBe('Hello world')
  })

  it('log_entry event appends to logEntries', () => {
    renderHook(() => useRunStream('run-1', 'prog-1'), { wrapper })
    const ws = getLatestWs()
    ws.simulateMessage({
      type: 'log_entry',
      stepId: 's1',
      level: 'info',
      message: 'Processing…',
      timestamp: '2026-02-22T00:00:00Z',
    })
    const { logEntries } = useUiStore.getState()
    expect(logEntries).toHaveLength(1)
    expect(logEntries[0].message).toBe('Processing…')
  })

  it('first tool_call event switches active bottom tab to logs', () => {
    useUiStore.setState({ activeBottomTab: 'output' })
    renderHook(() => useRunStream('run-1', 'prog-1'), { wrapper })
    const ws = getLatestWs()
    ws.simulateMessage({
      type: 'tool_call',
      phase: 'invoke',
      nodeId: 'n1',
      timestamp: '2026-02-22T00:00:00Z',
      data: { function: 'list_issues', tool_call_id: 'tc1', arguments: {} },
    })
    expect(useUiStore.getState().activeBottomTab).toBe('logs')
  })

  it('run_state: running clears run output', () => {
    useUiStore.setState({ llmOutput: 'stale', logEntries: [{ stepId: 's0', level: 'info', message: 'old', timestamp: '' }] })
    renderHook(() => useRunStream('run-1', 'prog-1'), { wrapper })
    const ws = getLatestWs()
    ws.simulateMessage({ type: 'run_state', state: 'running', runId: 'run-1' })
    expect(useUiStore.getState().llmOutput).toBe('')
    expect(useUiStore.getState().logEntries).toHaveLength(0)
  })

  it('run_state: completed sets activeRunId to null and fetches run detail', async () => {
    const { fetchRunDetail } = await import('../api/client')
    useUiStore.setState({ activeRunId: 'run-1' })
    const { unmount } = renderHook(() => useRunStream('run-1', 'prog-1'), { wrapper })
    const ws = getLatestWs()
    ws.simulateMessage({ type: 'run_state', state: 'completed', runId: 'run-1' })
    expect(useUiStore.getState().activeRunId).toBeNull()
    expect(fetchRunDetail).toHaveBeenCalledWith('run-1')
    unmount()
  })

  it('run_state: failed with step errors sets runError', async () => {
    const { fetchRunDetail } = await import('../api/client')
    vi.mocked(fetchRunDetail).mockResolvedValueOnce({
      id: 'run-1',
      programId: 'prog-1',
      state: 'failed',
      steps: [{ id: 's1', runId: 'run-1', nodeId: 'n1', state: 'failed', output: null, error: 'API key missing', startedAt: null, completedAt: null }],
      startedAt: null,
      completedAt: null,
    })
    renderHook(() => useRunStream('run-1', 'prog-1'), { wrapper })
    const ws = getLatestWs()
    ws.simulateMessage({ type: 'run_state', state: 'failed', runId: 'run-1' })
    // Wait for the fetchRunDetail promise to resolve
    await vi.waitFor(() => {
      expect(useUiStore.getState().runError).toBe('API key missing')
    })
  })

  it('cleanup on unmount closes the WebSocket', () => {
    const { unmount } = renderHook(() => useRunStream('run-1', 'prog-1'), { wrapper })
    const ws = getLatestWs()
    unmount()
    expect(ws.close).toHaveBeenCalled()
  })
})
