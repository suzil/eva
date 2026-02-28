import { useMemo } from 'react'
import {
  ReactFlow,
  ReactFlowProvider,
  BaseEdge,
  getBezierPath,
  type NodeProps,
  type EdgeProps,
  type Node,
  type Edge,
  type NodeTypes,
  type EdgeTypes,
} from '@xyflow/react'
import { Check, X, MessageSquare } from 'lucide-react'
import { useCanvasStore } from '../../store/canvasStore'
import { useUiStore } from '../../store/uiStore'
import type { EvaNodeData, Graph } from '../../types'
import { NODE_TYPE_COLORS, NODE_TYPE_LABELS } from '../../constants/nodeConstants'

// ---------------------------------------------------------------------------
// PreviewNode — simplified, non-interactive, dashed-border node
// ---------------------------------------------------------------------------

function PreviewNode({ data }: NodeProps<Node<EvaNodeData>>) {
  const color = NODE_TYPE_COLORS[data.nodeType.type] ?? '#00B4FF'
  const typeLabel = NODE_TYPE_LABELS[data.nodeType.type] ?? data.nodeType.type

  return (
    <div
      className="rounded-lg border border-dashed bg-terminal-800/60"
      style={{
        borderColor: color,
        minWidth: 140,
        opacity: 0.8,
        boxShadow: `0 0 10px ${color}33`,
        animation: 'glow-pulse 2s ease-in-out infinite',
      }}
    >
      <div className="flex items-center gap-2 px-3 py-2">
        {/* Left accent strip */}
        <div className="h-full w-1 shrink-0 self-stretch rounded-full" style={{ backgroundColor: color }} />
        <div className="min-w-0">
          <p
            className="text-[9px] font-semibold uppercase tracking-wider font-display"
            style={{ color }}
          >
            {typeLabel}
          </p>
          <p className="truncate text-xs text-terminal-200">{data.label}</p>
        </div>
      </div>
    </div>
  )
}

const previewNodeTypes: NodeTypes = {
  agent: PreviewNode,
  knowledge: PreviewNode,
  connector: PreviewNode,
  action: PreviewNode,
  trigger: PreviewNode,
}

// ---------------------------------------------------------------------------
// PreviewEdge — dashed, non-interactive
// ---------------------------------------------------------------------------

function PreviewEdge({ sourceX, sourceY, targetX, targetY, sourcePosition, targetPosition, data }: EdgeProps) {
  const [path] = getBezierPath({ sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition })
  const isDashed = (data as { category?: string } | undefined)?.category === 'resource'

  return (
    <BaseEdge
      path={path}
      style={{
        stroke: '#00B4FF',
        strokeWidth: 1.5,
        strokeDasharray: isDashed ? '6 3' : '8 4',
        opacity: 0.6,
      }}
    />
  )
}

const previewEdgeTypes: EdgeTypes = {
  data: PreviewEdge,
  resource: PreviewEdge,
}

// ---------------------------------------------------------------------------
// Graph-to-ReactFlow conversion
// ---------------------------------------------------------------------------

function graphToFlowElements(graph: Graph): { nodes: Node<EvaNodeData>[]; edges: Edge[] } {
  const nodes: Node<EvaNodeData>[] = Object.values(graph.nodes).map((n) => ({
    id: n.id,
    type: n.type.type,
    position: { x: n.posX, y: n.posY },
    data: { label: n.label, nodeType: n.type },
    draggable: false,
    selectable: false,
    connectable: false,
  }))

  const edges: Edge[] = graph.edges.map((e) => ({
    id: e.id,
    source: e.sourceNode,
    sourceHandle: e.sourcePort,
    target: e.targetNode,
    targetHandle: e.targetPort,
    type: e.category,
    data: { category: e.category },
    selectable: false,
    deletable: false,
    focusable: false,
  }))

  return { nodes, edges }
}

// ---------------------------------------------------------------------------
// Banner — floating top bar with Accept / Edit in Chat / Cancel
// ---------------------------------------------------------------------------

function PreviewBanner() {
  const graph = useCanvasStore((s) => s.previewOverlayGraph)!
  const loadGraph = useCanvasStore((s) => s.loadGraph)
  const markDirty = useCanvasStore((s) => s.markDirty)
  const setPreviewOverlayGraph = useCanvasStore((s) => s.setPreviewOverlayGraph)
  const currentProgramId = useCanvasStore((s) => s.currentProgramId)

  const setDetailPanelTab = useUiStore((s) => s.setDetailPanelTab)
  const setPrefillAssistantMessage = useUiStore((s) => s.setPrefillAssistantMessage)

  function handleAccept() {
    if (!currentProgramId) return
    loadGraph(graph, currentProgramId)
    markDirty()
    setPreviewOverlayGraph(null)
    setDetailPanelTab('inspector')
  }

  function handleEditInChat() {
    setPrefillAssistantMessage('Please revise: ')
    setDetailPanelTab('magi')
    setPreviewOverlayGraph(null)
  }

  function handleCancel() {
    setPreviewOverlayGraph(null)
  }

  return (
    <div className="absolute left-1/2 top-3 z-10 -translate-x-1/2">
      <div className="flex items-center gap-3 rounded-lg border border-warn-amber-500/40 bg-terminal-900/95 px-4 py-2 shadow-lg backdrop-blur-sm">
        {/* NERV caution stripe motif */}
        <div
          className="h-4 w-1.5 shrink-0 rounded-sm"
          style={{
            background: 'repeating-linear-gradient(45deg, #F5A623 0px, #F5A623 3px, #1A1B2E 3px, #1A1B2E 6px)',
          }}
        />
        <span className="text-xs font-display uppercase tracking-widest text-warn-amber-400">
          Preview Mode
        </span>
        <div className="flex items-center gap-2">
          <button
            type="button"
            onClick={handleAccept}
            disabled={!currentProgramId}
            className="flex items-center gap-1 rounded border border-eva-green-500/40 bg-eva-green-500/10 px-2.5 py-1 text-xs text-eva-green-400 transition-colors hover:bg-eva-green-500/20 disabled:cursor-not-allowed disabled:opacity-40"
          >
            <Check className="h-3 w-3" />
            Accept
          </button>
          <button
            type="button"
            onClick={handleEditInChat}
            className="flex items-center gap-1 rounded border border-terminal-600 px-2.5 py-1 text-xs text-terminal-300 transition-colors hover:border-terminal-500 hover:text-terminal-100"
          >
            <MessageSquare className="h-3 w-3" />
            Edit in Chat
          </button>
          <button
            type="button"
            onClick={handleCancel}
            className="flex items-center gap-1 rounded border border-terminal-600 px-2 py-1 text-xs text-terminal-400 transition-colors hover:border-nerv-red-500/40 hover:text-nerv-red-400"
          >
            <X className="h-3 w-3" />
            Cancel
          </button>
        </div>
      </div>
    </div>
  )
}

// ---------------------------------------------------------------------------
// Inner overlay — must be inside a ReactFlowProvider for useReactFlow
// ---------------------------------------------------------------------------

function OverlayInner() {
  const previewOverlayGraph = useCanvasStore((s) => s.previewOverlayGraph)

  const { nodes, edges } = useMemo(
    () => (previewOverlayGraph ? graphToFlowElements(previewOverlayGraph) : { nodes: [], edges: [] }),
    [previewOverlayGraph],
  )

  if (!previewOverlayGraph) return null

  return (
    <div className="absolute inset-0 z-20">
      {/* Translucent dimming backdrop */}
      <div className="absolute inset-0 bg-terminal-950/50 pointer-events-none" />

      {/* Read-only ReactFlow showing the proposed graph */}
      <ReactFlow
        nodes={nodes}
        edges={edges}
        nodeTypes={previewNodeTypes}
        edgeTypes={previewEdgeTypes}
        nodesDraggable={false}
        nodesConnectable={false}
        edgesReconnectable={false}
        elementsSelectable={false}
        panOnDrag
        zoomOnScroll
        fitView
        fitViewOptions={{ padding: 0.3 }}
        proOptions={{ hideAttribution: true }}
        style={{ background: 'transparent' }}
        className="pointer-events-auto"
      />

      {/* Floating banner */}
      <PreviewBanner />
    </div>
  )
}

// ---------------------------------------------------------------------------
// Public export — wraps in its own ReactFlowProvider to avoid conflicts
// ---------------------------------------------------------------------------

export function GraphPreviewOverlay() {
  const previewOverlayGraph = useCanvasStore((s) => s.previewOverlayGraph)
  if (!previewOverlayGraph) return null

  return (
    <ReactFlowProvider>
      <OverlayInner />
    </ReactFlowProvider>
  )
}
