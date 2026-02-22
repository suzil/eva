import { describe, it, expect, beforeEach } from 'vitest'
import { useCanvasStore } from './canvasStore'
import type { Graph } from '../types'

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const INITIAL_STATE = {
  nodes: [],
  edges: [],
  currentProgramId: null,
  selectedNodeId: null,
  selectedEdgeId: null,
  isDirty: false,
  nodeStepStates: {},
  nodeStepErrors: {},
  past: [],
  future: [],
  triggerFitView: false,
}

beforeEach(() => {
  useCanvasStore.setState(INITIAL_STATE)
})

// ---------------------------------------------------------------------------
// Minimal fixtures
// ---------------------------------------------------------------------------

const GRAPH: Graph = {
  nodes: {
    'n1': {
      id: 'n1',
      label: 'Agent',
      type: { type: 'agent', config: { provider: 'openai', model: 'gpt-4o', systemPrompt: '', temperature: 0.7, maxIterations: 5, responseFormat: 'text' } },
      posX: 100,
      posY: 200,
    },
    'n2': {
      id: 'n2',
      label: 'Trigger',
      type: { type: 'trigger', config: { type: 'manual' } },
      posX: 0,
      posY: 0,
    },
  },
  edges: [
    {
      id: 'e1',
      sourceNode: 'n2',
      sourcePort: 'out',
      targetNode: 'n1',
      targetPort: 'in',
      category: 'data',
    },
  ],
}

// ---------------------------------------------------------------------------
// loadGraph
// ---------------------------------------------------------------------------

describe('canvasStore — loadGraph', () => {
  it('converts API nodes dict to React Flow Node array', () => {
    useCanvasStore.getState().loadGraph(GRAPH, 'prog-1')
    const { nodes } = useCanvasStore.getState()
    expect(nodes).toHaveLength(2)
    const n1 = nodes.find((n) => n.id === 'n1')
    expect(n1).toBeDefined()
    expect(n1!.type).toBe('agent')
    expect(n1!.position).toEqual({ x: 100, y: 200 })
    expect(n1!.data.label).toBe('Agent')
  })

  it('converts API edges array to React Flow Edge array', () => {
    useCanvasStore.getState().loadGraph(GRAPH, 'prog-1')
    const { edges } = useCanvasStore.getState()
    expect(edges).toHaveLength(1)
    expect(edges[0].id).toBe('e1')
    expect(edges[0].source).toBe('n2')
    expect(edges[0].target).toBe('n1')
    expect(edges[0].sourceHandle).toBe('out')
    expect(edges[0].targetHandle).toBe('in')
    expect(edges[0].type).toBe('data')
  })

  it('resets isDirty and clears undo/redo history', () => {
    // Prime the store with some state first
    useCanvasStore.setState({ isDirty: true, past: [{ nodes: [], edges: [] }], future: [{ nodes: [], edges: [] }] })
    useCanvasStore.getState().loadGraph(GRAPH, 'prog-1')
    const { isDirty, past, future, currentProgramId } = useCanvasStore.getState()
    expect(isDirty).toBe(false)
    expect(past).toHaveLength(0)
    expect(future).toHaveLength(0)
    expect(currentProgramId).toBe('prog-1')
  })
})

// ---------------------------------------------------------------------------
// buildGraph
// ---------------------------------------------------------------------------

describe('canvasStore — buildGraph', () => {
  it('round-trips node id, label, type and position', () => {
    useCanvasStore.getState().loadGraph(GRAPH, 'prog-1')
    const built = useCanvasStore.getState().buildGraph()
    expect(built.nodes['n1']).toEqual({
      id: 'n1',
      label: 'Agent',
      type: GRAPH.nodes['n1'].type,
      posX: 100,
      posY: 200,
    })
  })

  it('round-trips edge sourceNode, targetNode, ports and category', () => {
    useCanvasStore.getState().loadGraph(GRAPH, 'prog-1')
    const built = useCanvasStore.getState().buildGraph()
    expect(built.edges).toHaveLength(1)
    expect(built.edges[0]).toEqual({
      id: 'e1',
      sourceNode: 'n2',
      sourcePort: 'out',
      targetNode: 'n1',
      targetPort: 'in',
      category: 'data',
    })
  })
})

// ---------------------------------------------------------------------------
// addNode / addEdge
// ---------------------------------------------------------------------------

describe('canvasStore — addNode / addEdge', () => {
  it('addNode pushes a history snapshot and marks isDirty', () => {
    const newNode = {
      id: 'n3',
      type: 'action' as const,
      position: { x: 50, y: 50 },
      data: {
        label: 'Action',
        nodeType: { type: 'action' as const, config: { type: 'op_template', template: '' } },
      },
    }
    useCanvasStore.getState().addNode(newNode)
    const { nodes, past, isDirty } = useCanvasStore.getState()
    expect(nodes).toHaveLength(1)
    expect(nodes[0].id).toBe('n3')
    expect(past).toHaveLength(1)
    expect(isDirty).toBe(true)
  })

  it('addEdge pushes a history snapshot and marks isDirty', () => {
    const newEdge = { id: 'e2', source: 'n1', target: 'n2', type: 'data' as const }
    useCanvasStore.getState().addEdge(newEdge)
    const { edges, past, isDirty } = useCanvasStore.getState()
    expect(edges).toHaveLength(1)
    expect(edges[0].id).toBe('e2')
    expect(past).toHaveLength(1)
    expect(isDirty).toBe(true)
  })
})

// ---------------------------------------------------------------------------
// undo / redo
// ---------------------------------------------------------------------------

describe('canvasStore — undo / redo', () => {
  it('undo restores previous nodes/edges and moves entry to future', () => {
    useCanvasStore.getState().loadGraph(GRAPH, 'prog-1')
    const nodesBefore = useCanvasStore.getState().nodes.slice()

    const extra = { id: 'n3', type: 'action' as const, position: { x: 0, y: 0 }, data: { label: 'X', nodeType: { type: 'action' as const, config: { type: 'op_template' as const, template: '' } } } }
    useCanvasStore.getState().addNode(extra)
    expect(useCanvasStore.getState().nodes).toHaveLength(3)

    useCanvasStore.getState().undo()
    const { nodes, future } = useCanvasStore.getState()
    expect(nodes).toHaveLength(nodesBefore.length)
    expect(future).toHaveLength(1)
  })

  it('undo is a no-op when history is empty', () => {
    useCanvasStore.getState().loadGraph(GRAPH, 'prog-1')
    // history was cleared by loadGraph
    useCanvasStore.getState().undo()
    expect(useCanvasStore.getState().nodes).toHaveLength(2)
  })

  it('redo reapplies the undone snapshot', () => {
    useCanvasStore.getState().loadGraph(GRAPH, 'prog-1')
    const extra = { id: 'n3', type: 'action' as const, position: { x: 0, y: 0 }, data: { label: 'X', nodeType: { type: 'action' as const, config: { type: 'op_template' as const, template: '' } } } }
    useCanvasStore.getState().addNode(extra)
    useCanvasStore.getState().undo()
    expect(useCanvasStore.getState().nodes).toHaveLength(2)

    useCanvasStore.getState().redo()
    expect(useCanvasStore.getState().nodes).toHaveLength(3)
    expect(useCanvasStore.getState().future).toHaveLength(0)
  })
})

// ---------------------------------------------------------------------------
// Step state overlays
// ---------------------------------------------------------------------------

describe('canvasStore — step state overlays', () => {
  it('setNodeStepState updates nodeStepStates and node.data.stepState', () => {
    useCanvasStore.getState().loadGraph(GRAPH, 'prog-1')
    useCanvasStore.getState().setNodeStepState('n1', 'running')
    const { nodeStepStates, nodes } = useCanvasStore.getState()
    expect(nodeStepStates['n1']).toBe('running')
    const n1 = nodes.find((n) => n.id === 'n1')!
    expect(n1.data.stepState).toBe('running')
  })

  it('setNodeStepErrors bulk-sets the error map', () => {
    useCanvasStore.getState().loadGraph(GRAPH, 'prog-1')
    useCanvasStore.getState().setNodeStepErrors({ n1: 'API key missing', n2: 'Timeout' })
    const { nodeStepErrors } = useCanvasStore.getState()
    expect(nodeStepErrors).toEqual({ n1: 'API key missing', n2: 'Timeout' })
  })

  it('clearRunState removes stepState from node data and resets maps', () => {
    useCanvasStore.getState().loadGraph(GRAPH, 'prog-1')
    useCanvasStore.getState().setNodeStepState('n1', 'completed')
    useCanvasStore.getState().setNodeStepErrors({ n1: 'err' })
    useCanvasStore.getState().clearRunState()

    const { nodeStepStates, nodeStepErrors, nodes } = useCanvasStore.getState()
    expect(nodeStepStates).toEqual({})
    expect(nodeStepErrors).toEqual({})
    const n1 = nodes.find((n) => n.id === 'n1')!
    expect(n1.data.stepState).toBeUndefined()
  })
})
