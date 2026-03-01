import { useMemo, useState } from 'react'
import {
  ReactFlow,
  ReactFlowProvider,
  BaseEdge,
  getBezierPath,
  Handle,
  Position,
  type NodeProps,
  type EdgeProps,
  type Node,
  type Edge,
  type NodeTypes,
  type EdgeTypes,
  type NodeMouseHandler,
} from '@xyflow/react'
import { Check, X, MessageSquare, ChevronRight } from 'lucide-react'
import { useCanvasStore } from '../../store/canvasStore'
import { useUiStore } from '../../store/uiStore'
import { useSaveGraph, usePatchProgram, useProgram } from '../../api/hooks'
import type { EvaNodeData, Graph, NodeType } from '../../types'
import { NODE_TYPE_COLORS, NODE_TYPE_LABELS } from '../../constants/nodeConstants'

// ---------------------------------------------------------------------------
// PreviewNode — dashed-border node, highlights when selected
// ---------------------------------------------------------------------------

function PreviewNode({ data, selected }: NodeProps<Node<EvaNodeData>>) {
  const color = NODE_TYPE_COLORS[data.nodeType.type] ?? '#00B4FF'
  const typeLabel = NODE_TYPE_LABELS[data.nodeType.type] ?? data.nodeType.type

  const handleStyle = { opacity: 0, pointerEvents: 'none' as const, width: 6, height: 6 }

  return (
    <div
      className="rounded-lg border bg-terminal-800/60 cursor-pointer transition-all"
      style={{
        borderStyle: selected ? 'solid' : 'dashed',
        borderColor: color,
        minWidth: 140,
        opacity: selected ? 1 : 0.85,
        boxShadow: selected
          ? `0 0 16px ${color}66, 0 0 4px ${color}44`
          : `0 0 10px ${color}33`,
        animation: selected ? undefined : 'glow-pulse 2s ease-in-out infinite',
      }}
    >
      {/* Invisible handles so ReactFlow can route edges to/from this node */}
      <Handle type="target" position={Position.Left}  id="input"  style={handleStyle} />
      <Handle type="source" position={Position.Right} id="output" style={handleStyle} />

      <div className="flex items-center gap-2 px-3 py-2">
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
        {selected && <ChevronRight className="ml-auto h-3 w-3 shrink-0 text-terminal-400" />}
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
// Note: sourceHandle/targetHandle are omitted so ReactFlow connects nodes at
// their centre by default (PreviewNode has no named handles defined).
// ---------------------------------------------------------------------------

function graphToFlowElements(graph: Graph): { nodes: Node<EvaNodeData>[]; edges: Edge[] } {
  const nodes: Node<EvaNodeData>[] = Object.values(graph.nodes).map((n) => ({
    id: n.id,
    type: n.type.type,
    position: { x: n.posX, y: n.posY },
    data: { label: n.label, nodeType: n.type },
    draggable: false,
    connectable: false,
  }))

  const edges: Edge[] = graph.edges.map((e) => ({
    id: e.id,
    source: e.sourceNode,
    sourceHandle: 'output',
    target: e.targetNode,
    targetHandle: 'input',
    type: e.category,
    data: { category: e.category },
    selectable: false,
    deletable: false,
    focusable: false,
  }))

  return { nodes, edges }
}

// ---------------------------------------------------------------------------
// NodeDetailPanel — shown in bottom-left when a preview node is selected
// ---------------------------------------------------------------------------

function nodeConfigRows(nodeType: NodeType): { label: string; value: string }[] {
  switch (nodeType.type) {
    case 'trigger': {
      const cfg = nodeType.config
      const rows: { label: string; value: string }[] = [{ label: 'type', value: cfg.type }]
      if (cfg.schedule) rows.push({ label: 'schedule', value: cfg.schedule })
      if (cfg.eventFilter) rows.push({ label: 'event filter', value: cfg.eventFilter })
      return rows
    }
    case 'agent': {
      const cfg = nodeType.config
      const rows: { label: string; value: string }[] = [
        { label: 'model', value: cfg.model },
        { label: 'format', value: cfg.responseFormat },
        { label: 'temperature', value: String(cfg.temperature) },
        { label: 'max iterations', value: String(cfg.maxIterations) },
      ]
      if (cfg.systemPrompt) {
        const prompt = cfg.systemPrompt.length > 120
          ? cfg.systemPrompt.slice(0, 117) + '…'
          : cfg.systemPrompt
        rows.push({ label: 'system prompt', value: prompt })
      }
      return rows
    }
    case 'knowledge': {
      const cfg = nodeType.config
      const srcType = cfg.source.type.replace(/^_/, '')
      const rows: { label: string; value: string }[] = [
        { label: 'source', value: srcType },
        { label: 'format', value: cfg.format },
        { label: 'refresh', value: cfg.refreshPolicy.type },
      ]
      if ('value' in cfg.source && typeof cfg.source.value === 'string' && cfg.source.value) {
        const val = cfg.source.value.length > 120
          ? cfg.source.value.slice(0, 117) + '…'
          : cfg.source.value
        rows.push({ label: 'content', value: val })
      }
      return rows
    }
    case 'action':
      return [{ label: 'operation', value: nodeType.config.operation }]
    case 'connector':
      return [{ label: 'system', value: nodeType.config.system }]
  }
}

interface NodeDetailPanelProps {
  nodeType: NodeType
  label: string
  onClose: () => void
}

function NodeDetailPanel({ nodeType, label, onClose }: NodeDetailPanelProps) {
  const color = NODE_TYPE_COLORS[nodeType.type] ?? '#00B4FF'
  const typeLabel = NODE_TYPE_LABELS[nodeType.type] ?? nodeType.type
  const rows = nodeConfigRows(nodeType)

  return (
    <div className="absolute bottom-14 left-4 z-30 w-64 rounded-lg border border-terminal-600 bg-terminal-900/95 shadow-xl backdrop-blur-sm">
      {/* Header */}
      <div
        className="flex items-center justify-between border-b border-terminal-700 px-3 py-2"
        style={{ borderLeftColor: color, borderLeftWidth: 3 }}
      >
        <div>
          <p className="text-[9px] font-display font-semibold uppercase tracking-widest" style={{ color }}>
            {typeLabel}
          </p>
          <p className="text-xs text-terminal-100 font-medium">{label}</p>
        </div>
        <button
          type="button"
          onClick={onClose}
          className="rounded p-0.5 text-terminal-500 transition-colors hover:text-terminal-200"
        >
          <X className="h-3 w-3" />
        </button>
      </div>

      {/* Config rows */}
      {rows.length > 0 && (
        <div className="space-y-1 px-3 py-2">
          {rows.map((r) => (
            <div key={r.label} className="flex flex-col gap-0.5">
              <span className="text-[9px] uppercase tracking-wider text-terminal-500 font-display">{r.label}</span>
              <span className="break-words text-[11px] text-terminal-200 font-mono leading-relaxed">{r.value}</span>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}

// ---------------------------------------------------------------------------
// Banner — floating top bar with Accept / Edit in Chat / Cancel
// ---------------------------------------------------------------------------

function PreviewBanner() {
  const graph = useCanvasStore((s) => s.previewOverlayGraph)!
  const summary = useCanvasStore((s) => s.previewOverlaySummary)
  const proposedName = useCanvasStore((s) => s.previewOverlayName)
  const loadGraph = useCanvasStore((s) => s.loadGraph)
  const markDirty = useCanvasStore((s) => s.markDirty)
  const setPreviewOverlayGraph = useCanvasStore((s) => s.setPreviewOverlayGraph)
  const setAcceptedPreviewGraph = useCanvasStore((s) => s.setAcceptedPreviewGraph)
  const currentProgramId = useCanvasStore((s) => s.currentProgramId)

  const setDetailPanelTab = useUiStore((s) => s.setDetailPanelTab)
  const setPrefillAssistantMessage = useUiStore((s) => s.setPrefillAssistantMessage)

  const saveMutation = useSaveGraph(currentProgramId ?? '')
  const patchMutation = usePatchProgram(currentProgramId ?? '')
  const { data: currentProgram } = useProgram(currentProgramId ?? '')

  function handleAccept() {
    if (!currentProgramId) return
    loadGraph(graph, currentProgramId)
    setPreviewOverlayGraph(null)
    setAcceptedPreviewGraph(graph)
    setDetailPanelTab('inspector')
    saveMutation.mutate(graph, { onError: () => markDirty() })
    const currentName = currentProgram?.name ?? ''
    if (proposedName && (/^untitled$/i.test(currentName.trim()) || currentName.trim() === '')) {
      patchMutation.mutate({ name: proposedName, description: summary ?? undefined })
    }
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
  const [selectedNodeId, setSelectedNodeId] = useState<string | null>(null)

  const { nodes, edges } = useMemo(
    () => (previewOverlayGraph ? graphToFlowElements(previewOverlayGraph) : { nodes: [], edges: [] }),
    [previewOverlayGraph],
  )

  const onNodeClick: NodeMouseHandler<Node<EvaNodeData>> = (_e, node) => {
    setSelectedNodeId((prev) => (prev === node.id ? null : node.id))
  }

  if (!previewOverlayGraph) return null

  const selectedDomainNode = selectedNodeId ? previewOverlayGraph.nodes[selectedNodeId] : null

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
        onNodeClick={onNodeClick}
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

      {/* Node detail panel */}
      {selectedDomainNode && (
        <NodeDetailPanel
          nodeType={selectedDomainNode.type}
          label={selectedDomainNode.label}
          onClose={() => setSelectedNodeId(null)}
        />
      )}

      {/* Click-a-node hint — fades once user has clicked */}
      {!selectedNodeId && nodes.length > 0 && (
        <div className="pointer-events-none absolute bottom-4 left-1/2 -translate-x-1/2">
          <p className="rounded border border-terminal-700/60 bg-terminal-900/80 px-3 py-1 text-[10px] text-terminal-500 backdrop-blur-sm">
            Click a node to inspect its configuration
          </p>
        </div>
      )}
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
