import { create } from 'zustand'
import {
  applyNodeChanges,
  applyEdgeChanges,
  type Node,
  type Edge,
  type NodeChange,
  type EdgeChange,
} from '@xyflow/react'
import type { EvaNodeData, Graph, NodeId, NodeType, PortCategory, Step, StepState } from '../types'

// ---------------------------------------------------------------------------
// History
// ---------------------------------------------------------------------------

interface HistoryEntry {
  nodes: Node<EvaNodeData>[]
  edges: Edge[]
}

const HISTORY_LIMIT = 50

// ---------------------------------------------------------------------------
// Store shape
// ---------------------------------------------------------------------------

interface CanvasState {
  nodes: Node<EvaNodeData>[]
  edges: Edge[]
  currentProgramId: string | null
  selectedNodeId: string | null
  selectedEdgeId: string | null
  isDirty: boolean

  /** Per-node step states during a run. Drives BaseNode rings/badges and edge animation. */
  nodeStepStates: Record<NodeId, StepState>
  /** Per-node step error messages, populated from RunDetail when a run finishes. */
  nodeStepErrors: Record<NodeId, string>

  /** Undo history — snapshots before mutating operations. */
  past: HistoryEntry[]
  /** Redo stack — snapshots that can be re-applied. */
  future: HistoryEntry[]

  /** Set to true by setLayoutedNodes; CanvasInner watches it to call fitView(). */
  triggerFitView: boolean

  loadGraph: (graph: Graph, programId: string) => void
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

  /** Push current nodes+edges onto the undo stack and clear the redo stack. */
  snapshot: () => void
  undo: () => void
  redo: () => void

  /** Replace node positions after auto-layout; triggers fitView on next render. */
  setLayoutedNodes: (nodes: Node<EvaNodeData>[]) => void
  setTriggerFitView: (value: boolean) => void

  /** Update a single node's step state and sync it into EvaNodeData for BaseNode rendering. */
  setNodeStepState: (nodeId: NodeId, state: StepState) => void
  /** Bulk-set step errors (keyed by nodeId) after fetching RunDetail. */
  setNodeStepErrors: (errors: Record<NodeId, string>) => void
  /** Load step states from a completed RunDetail into the canvas overlay. Clears previous states first. */
  loadRunSteps: (steps: Step[]) => void
  /** Reset all run-time overlays (step states + errors) after a run finishes or a new graph loads. */
  clearRunState: () => void

  /** Convert current store state to API Graph shape for PUT /programs/:id/graph */
  buildGraph: () => Graph
}

export const useCanvasStore = create<CanvasState>((set, get) => ({
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

  loadGraph: (graph, programId) => {
    const nodes: Node<EvaNodeData>[] = Object.values(graph.nodes).map((n) => ({
      id: n.id,
      type: n.type.type,
      position: { x: n.posX, y: n.posY },
      data: { label: n.label, nodeType: n.type },
    }))
    const edges: Edge[] = graph.edges.map((e) => ({
      id: e.id,
      source: e.sourceNode,
      sourceHandle: e.sourcePort,
      target: e.targetNode,
      targetHandle: e.targetPort,
      type: e.category,
    }))
    set({
      nodes,
      edges,
      currentProgramId: programId,
      isDirty: false,
      selectedNodeId: null,
      selectedEdgeId: null,
      nodeStepStates: {},
      nodeStepErrors: {},
      past: [],
      future: [],
    })
  },

  applyNodeChanges: (changes) => {
    const hasRemoval = changes.some((c) => c.type === 'remove')
    if (hasRemoval) get().snapshot()
    set((s) => ({
      nodes: applyNodeChanges(changes, s.nodes),
      isDirty: true,
    }))
  },

  applyEdgeChanges: (changes) => {
    const hasRemoval = changes.some((c) => c.type === 'remove')
    if (hasRemoval) get().snapshot()
    set((s) => ({
      edges: applyEdgeChanges(changes, s.edges),
      isDirty: true,
    }))
  },

  addNode: (node) => {
    get().snapshot()
    set((s) => ({ nodes: [...s.nodes, node], isDirty: true }))
  },

  addEdge: (edge) => {
    get().snapshot()
    set((s) => ({ edges: [...s.edges, edge], isDirty: true }))
  },

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
    set((state) => ({
      selectedNodeId: id,
      selectedEdgeId: null,
      nodes: state.nodes.map((n) => ({ ...n, selected: n.id === id })),
    })),

  setSelectedEdge: (id) =>
    set({ selectedEdgeId: id, selectedNodeId: null }),

  clearSelection: () =>
    set((state) => ({
      selectedNodeId: null,
      selectedEdgeId: null,
      nodes: state.nodes.map((n) => ({ ...n, selected: false })),
    })),

  markClean: () => set({ isDirty: false }),

  snapshot: () => {
    const { nodes, edges, past } = get()
    set({
      past: [...past.slice(-HISTORY_LIMIT + 1), { nodes, edges }],
      future: [],
    })
  },

  undo: () => {
    const { past, nodes, edges, future } = get()
    if (past.length === 0) return
    const prev = past[past.length - 1]
    set({
      past: past.slice(0, -1),
      future: [{ nodes, edges }, ...future],
      nodes: prev.nodes,
      edges: prev.edges,
      isDirty: true,
    })
  },

  redo: () => {
    const { past, nodes, edges, future } = get()
    if (future.length === 0) return
    const next = future[0]
    set({
      past: [...past, { nodes, edges }],
      future: future.slice(1),
      nodes: next.nodes,
      edges: next.edges,
      isDirty: true,
    })
  },

  setLayoutedNodes: (nodes) => {
    get().snapshot()
    set({ nodes, isDirty: true, triggerFitView: true })
  },

  setTriggerFitView: (value) => set({ triggerFitView: value }),

  setNodeStepState: (nodeId, state) =>
    set((s) => ({
      nodeStepStates: { ...s.nodeStepStates, [nodeId]: state },
      nodes: s.nodes.map((n) =>
        n.id === nodeId ? { ...n, data: { ...n.data, stepState: state } } : n,
      ),
    })),

  setNodeStepErrors: (errors) => set({ nodeStepErrors: errors }),

  loadRunSteps: (steps) =>
    set((s) => {
      const newStepStates: Record<NodeId, StepState> = {}
      const newStepErrors: Record<NodeId, string> = {}
      for (const step of steps) {
        newStepStates[step.nodeId] = step.state
        if (step.error) newStepErrors[step.nodeId] = step.error
      }
      return {
        nodeStepStates: newStepStates,
        nodeStepErrors: newStepErrors,
        nodes: s.nodes.map((n) => {
          const state = newStepStates[n.id]
          if (state === undefined) {
            if (n.data.stepState === undefined) return n
            const { stepState: _, ...rest } = n.data
            return { ...n, data: rest as EvaNodeData }
          }
          return { ...n, data: { ...n.data, stepState: state } }
        }),
      }
    }),

  clearRunState: () =>
    set((s) => ({
      nodeStepStates: {},
      nodeStepErrors: {},
      nodes: s.nodes.map((n) => {
        if (n.data.stepState === undefined) return n
        const { stepState: _, ...rest } = n.data
        return { ...n, data: rest as EvaNodeData }
      }),
    })),

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
