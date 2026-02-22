import { create } from 'zustand'
import {
  applyNodeChanges,
  applyEdgeChanges,
  type Node,
  type Edge,
  type NodeChange,
  type EdgeChange,
} from '@xyflow/react'
import type { EvaNodeData, Graph, NodeType, PortCategory } from '../types'

// ---------------------------------------------------------------------------
// Demo nodes — one of each type. EVA-22 replaces this with backend load.
// ---------------------------------------------------------------------------

const DEMO_NODES: Node<EvaNodeData>[] = [
  {
    id: 'trigger-1',
    type: 'trigger',
    position: { x: 60, y: 160 },
    data: {
      label: 'Weekly Trigger',
      nodeType: { type: 'trigger', config: { type: 'cron', schedule: '0 9 * * 1' } },
    },
  },
  {
    id: 'knowledge-1',
    type: 'knowledge',
    position: { x: 60, y: 300 },
    data: {
      label: 'Team Context',
      nodeType: {
        type: 'knowledge',
        config: {
          source: { type: '_inline_text', value: '' },
          format: 'text',
          refreshPolicy: { type: 'static' },
        },
      },
    },
  },
  {
    id: 'connector-1',
    type: 'connector',
    position: { x: 60, y: 420 },
    data: {
      label: 'Linear',
      nodeType: { type: 'connector', config: { system: 'linear', actionFilter: [] } },
    },
  },
  {
    id: 'agent-1',
    type: 'agent',
    position: { x: 340, y: 200 },
    data: {
      label: 'Summarizer',
      nodeType: {
        type: 'agent',
        config: {
          model: 'gpt-4o',
          systemPrompt: '',
          responseFormat: 'text',
          temperature: 0.7,
          maxIterations: 5,
        },
      },
      stepState: 'running',
    },
  },
  {
    id: 'action-1',
    type: 'action',
    position: { x: 600, y: 220 },
    data: {
      label: 'Format Report',
      nodeType: {
        type: 'action',
        config: { operation: 'template', parameters: {}, errorHandling: { mode: 'fail' } },
      },
    },
  },
]

const DEMO_EDGES: Edge[] = [
  {
    id: 'e1',
    type: 'data',
    source: 'trigger-1',
    sourceHandle: 'event',
    target: 'agent-1',
    targetHandle: 'instruction',
  },
  {
    id: 'e2',
    type: 'resource',
    source: 'knowledge-1',
    sourceHandle: 'content',
    target: 'agent-1',
    targetHandle: 'context',
  },
  {
    id: 'e3',
    type: 'resource',
    source: 'connector-1',
    sourceHandle: 'tools',
    target: 'agent-1',
    targetHandle: 'tools',
  },
  {
    id: 'e4',
    type: 'data',
    source: 'agent-1',
    sourceHandle: 'output',
    target: 'action-1',
    targetHandle: 'input',
  },
]

// ---------------------------------------------------------------------------
// Store shape
// ---------------------------------------------------------------------------

interface CanvasState {
  nodes: Node<EvaNodeData>[]
  edges: Edge[]
  selectedNodeId: string | null
  selectedEdgeId: string | null
  isDirty: boolean

  applyNodeChanges: (changes: NodeChange<Node<EvaNodeData>>[]) => void
  applyEdgeChanges: (changes: EdgeChange[]) => void
  addNode: (node: Node<EvaNodeData>) => void
  addEdge: (edge: Edge) => void
  updateNodeLabel: (id: string, label: string) => void
  updateNodeConfig: (id: string, config: NodeType['config']) => void
  setSelectedNode: (id: string | null) => void
  setSelectedEdge: (id: string | null) => void
  clearSelection: () => void
  markClean: () => void

  /** Convert current store state to API Graph shape for PUT /programs/:id/graph */
  buildGraph: () => Graph
}

export const useCanvasStore = create<CanvasState>((set, get) => ({
  nodes: DEMO_NODES,
  edges: DEMO_EDGES,
  selectedNodeId: null,
  selectedEdgeId: null,
  isDirty: false,

  applyNodeChanges: (changes) =>
    set((s) => ({
      nodes: applyNodeChanges(changes, s.nodes),
      isDirty: true,
    })),

  applyEdgeChanges: (changes) =>
    set((s) => ({
      edges: applyEdgeChanges(changes, s.edges),
      isDirty: true,
    })),

  addNode: (node) =>
    set((s) => ({ nodes: [...s.nodes, node], isDirty: true })),

  addEdge: (edge) =>
    set((s) => ({ edges: [...s.edges, edge], isDirty: true })),

  updateNodeLabel: (id, label) =>
    set((s) => ({
      nodes: s.nodes.map((n) =>
        n.id === id ? { ...n, data: { ...n.data, label } } : n,
      ),
      isDirty: true,
    })),

  updateNodeConfig: (id, config) =>
    set((s) => ({
      nodes: s.nodes.map((n) => {
        if (n.id !== id) return n
        const nodeType = { ...n.data.nodeType, config } as NodeType
        return { ...n, data: { ...n.data, nodeType } }
      }),
      isDirty: true,
    })),

  setSelectedNode: (id) =>
    set({ selectedNodeId: id, selectedEdgeId: null }),

  setSelectedEdge: (id) =>
    set({ selectedEdgeId: id, selectedNodeId: null }),

  clearSelection: () =>
    set({ selectedNodeId: null, selectedEdgeId: null }),

  markClean: () => set({ isDirty: false }),

  buildGraph: () => {
    const { nodes, edges } = get()
    const graphNodes: Graph['nodes'] = {}
    for (const n of nodes) {
      graphNodes[n.id] = {
        id: n.id,
        label: n.data.label,
        type: n.data.nodeType,
        posX: n.position.x,
        posY: n.position.y,
      }
    }
    const graphEdges: Graph['edges'] = edges.map((e) => ({
      id: e.id,
      sourceNode: e.source,
      sourcePort: e.sourceHandle ?? '',
      targetNode: e.target,
      targetPort: e.targetHandle ?? '',
      category: (e.type ?? 'data') as PortCategory,
    }))
    return { nodes: graphNodes, edges: graphEdges }
  },
}))
