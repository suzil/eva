import { useState } from 'react'
import { GitBranch, Eye, Check, RotateCcw } from 'lucide-react'
import { useCanvasStore } from '../../store/canvasStore'
import { useUiStore } from '../../store/uiStore'
import type { Graph, Node } from '../../types'
import { NODE_TYPE_COLORS, NODE_TYPE_LABELS } from '../../constants/nodeConstants'

interface GraphProposalCardProps {
  graph: Graph
  summary: string
}

// ---------------------------------------------------------------------------
// Client-side validation
// ---------------------------------------------------------------------------

function validateProposedGraph(graph: Graph): string[] {
  const errors: string[] = []
  const nodes = Object.values(graph.nodes)
  if (nodes.length === 0) {
    errors.push('Proposed graph has no nodes')
    return errors
  }
  const hasTrigger = nodes.some((n) => n.type.type === 'trigger')
  if (!hasTrigger) errors.push('Graph requires at least one Trigger node')
  if (nodes.length > 1 && graph.edges.length === 0)
    errors.push('Nodes are not connected — no edges defined')
  return errors
}

// ---------------------------------------------------------------------------
// Mini SVG diagram
// ---------------------------------------------------------------------------

const SVG_W = 220
const SVG_H = 110
const NODE_W = 52
const NODE_H = 20
const PADDING = 12

function MiniGraphDiagram({ graph }: { graph: Graph }) {
  const nodes = Object.values(graph.nodes)
  if (nodes.length === 0) return null

  // Compute bounding box of node positions
  const xs = nodes.map((n) => n.posX)
  const ys = nodes.map((n) => n.posY)
  const minX = Math.min(...xs)
  const minY = Math.min(...ys)
  const maxX = Math.max(...xs) + NODE_W
  const maxY = Math.max(...ys) + NODE_H

  const rangeX = Math.max(maxX - minX, 1)
  const rangeY = Math.max(maxY - minY, 1)

  const usableW = SVG_W - PADDING * 2 - NODE_W
  const usableH = SVG_H - PADDING * 2 - NODE_H

  function toSvgX(x: number) {
    return PADDING + ((x - minX) / rangeX) * usableW
  }
  function toSvgY(y: number) {
    return PADDING + ((y - minY) / rangeY) * usableH
  }

  // Position map for edge rendering
  const nodePos: Record<string, { cx: number; cy: number }> = {}
  nodes.forEach((n) => {
    nodePos[n.id] = {
      cx: toSvgX(n.posX) + NODE_W / 2,
      cy: toSvgY(n.posY) + NODE_H / 2,
    }
  })

  return (
    <svg
      width={SVG_W}
      height={SVG_H}
      viewBox={`0 0 ${SVG_W} ${SVG_H}`}
      className="overflow-visible rounded"
    >
      {/* Edges */}
      {graph.edges.map((e) => {
        const src = nodePos[e.sourceNode]
        const tgt = nodePos[e.targetNode]
        if (!src || !tgt) return null
        const color = e.category === 'resource' ? '#4F5070' : '#363755'
        return (
          <line
            key={e.id}
            x1={src.cx}
            y1={src.cy}
            x2={tgt.cx}
            y2={tgt.cy}
            stroke={color}
            strokeWidth={1}
            strokeDasharray={e.category === 'resource' ? '3 2' : undefined}
            markerEnd="url(#arrow)"
          />
        )
      })}

      {/* Arrow marker */}
      <defs>
        <marker id="arrow" markerWidth="6" markerHeight="6" refX="5" refY="3" orient="auto">
          <path d="M0,0 L0,6 L6,3 z" fill="#4F5070" />
        </marker>
      </defs>

      {/* Nodes */}
      {nodes.map((n) => {
        const x = toSvgX(n.posX)
        const y = toSvgY(n.posY)
        const color = NODE_TYPE_COLORS[n.type.type] ?? '#4F5070'
        const label = n.label.length > 10 ? n.label.slice(0, 9) + '…' : n.label
        const typeLabel = NODE_TYPE_LABELS[n.type.type] ?? n.type.type
        return (
          <g key={n.id}>
            <rect
              x={x}
              y={y}
              width={NODE_W}
              height={NODE_H}
              rx={3}
              fill="#1A1B2E"
              stroke={color}
              strokeWidth={1}
              opacity={0.9}
            />
            {/* Left accent strip */}
            <rect x={x} y={y} width={3} height={NODE_H} rx={1} fill={color} />
            <text
              x={x + 7}
              y={y + 8}
              fontSize={6}
              fill={color}
              fontFamily="monospace"
              fontWeight="600"
              textAnchor="start"
              dominantBaseline="middle"
            >
              {typeLabel.toUpperCase()}
            </text>
            <text
              x={x + 7}
              y={y + 15}
              fontSize={6}
              fill="#A0A0C0"
              fontFamily="monospace"
              textAnchor="start"
              dominantBaseline="middle"
            >
              {label}
            </text>
          </g>
        )
      })}
    </svg>
  )
}

// ---------------------------------------------------------------------------
// Node summary list (fallback when nodes have no spread positions)
// ---------------------------------------------------------------------------

function NodeList({ nodes }: { nodes: Node[] }) {
  return (
    <div className="flex flex-wrap gap-1">
      {nodes.map((n) => {
        const color = NODE_TYPE_COLORS[n.type.type] ?? '#4F5070'
        const typeLabel = NODE_TYPE_LABELS[n.type.type] ?? n.type.type
        return (
          <span
            key={n.id}
            className="rounded px-1.5 py-0.5 text-[10px] font-mono"
            style={{ backgroundColor: `${color}22`, color, border: `1px solid ${color}44` }}
          >
            {typeLabel}: {n.label}
          </span>
        )
      })}
    </div>
  )
}

// ---------------------------------------------------------------------------
// Main card
// ---------------------------------------------------------------------------

export function GraphProposalCard({ graph, summary }: GraphProposalCardProps) {
  const [validationErrors, setValidationErrors] = useState<string[]>([])
  const [accepted, setAccepted] = useState(false)

  const setPreviewOverlayGraph = useCanvasStore((s) => s.setPreviewOverlayGraph)
  const loadGraph = useCanvasStore((s) => s.loadGraph)
  const markDirty = useCanvasStore((s) => s.markDirty)
  const currentProgramId = useCanvasStore((s) => s.currentProgramId)
  const previewOverlayGraph = useCanvasStore((s) => s.previewOverlayGraph)
  const isPreviewActive = previewOverlayGraph !== null

  const setDetailPanelTab = useUiStore((s) => s.setDetailPanelTab)
  const setPrefillAssistantMessage = useUiStore((s) => s.setPrefillAssistantMessage)

  const nodes = Object.values(graph.nodes)

  function handlePreview() {
    setValidationErrors([])
    setPreviewOverlayGraph(graph)
    // Switch to inspector so the canvas is fully visible
    setDetailPanelTab('inspector')
  }

  function handleAccept() {
    const errors = validateProposedGraph(graph)
    if (errors.length > 0) {
      setValidationErrors(errors)
      return
    }
    setValidationErrors([])
    if (!currentProgramId) {
      setValidationErrors(['No program is open — open a program before accepting'])
      return
    }
    loadGraph(graph, currentProgramId)
    markDirty()
    setPreviewOverlayGraph(null)
    setAccepted(true)
    setDetailPanelTab('inspector')
  }

  function handleRevise() {
    setPrefillAssistantMessage('Please revise: ')
    setDetailPanelTab('magi')
  }

  if (accepted) {
    return (
      <div className="mx-3 rounded border border-eva-green-500/30 bg-eva-green-500/5 px-3 py-2">
        <div className="flex items-center gap-1.5 text-xs text-eva-green-500">
          <Check className="h-3.5 w-3.5" />
          <span className="font-display uppercase tracking-widest">Graph accepted — canvas updated</span>
        </div>
      </div>
    )
  }

  return (
    <div className="mx-3 rounded border border-terminal-600 bg-terminal-900">
      {/* Header */}
      <div className="flex items-center gap-1.5 border-b border-terminal-700 px-3 py-2 text-xs font-display uppercase tracking-widest text-magi-blue-400">
        <GitBranch className="h-3.5 w-3.5" />
        Graph Proposal
      </div>

      {/* Summary */}
      <div className="px-3 pt-2 pb-1">
        <p className="text-xs text-terminal-200 whitespace-pre-wrap">{summary}</p>
      </div>

      {/* Mini diagram */}
      <div className="px-3 pb-2">
        {nodes.length > 0 && (
          <div className="mt-1.5 overflow-hidden rounded border border-terminal-700 bg-terminal-950">
            <MiniGraphDiagram graph={graph} />
          </div>
        )}
        {nodes.length > 0 && (
          <div className="mt-1.5">
            <NodeList nodes={nodes} />
          </div>
        )}
      </div>

      {/* Validation errors */}
      {validationErrors.length > 0 && (
        <div className="mx-3 mb-2 rounded border border-nerv-red-500/40 bg-nerv-red-500/5 px-2 py-1.5">
          {validationErrors.map((err) => (
            <p key={err} className="text-[11px] text-nerv-red-400">
              {err}
            </p>
          ))}
        </div>
      )}

      {/* Action bar */}
      <div className="flex items-center gap-2 border-t border-terminal-700 px-3 py-2">
        <button
          type="button"
          onClick={handlePreview}
          className={[
            'flex items-center gap-1 rounded px-2 py-1 text-xs transition-colors',
            isPreviewActive
              ? 'bg-magi-blue-500/20 text-magi-blue-300 border border-magi-blue-500/40'
              : 'border border-terminal-600 text-terminal-300 hover:border-magi-blue-500/60 hover:text-magi-blue-300',
          ].join(' ')}
        >
          <Eye className="h-3 w-3" />
          Preview on Canvas
        </button>
        <button
          type="button"
          onClick={handleAccept}
          className="flex items-center gap-1 rounded border border-eva-green-500/40 bg-eva-green-500/10 px-2 py-1 text-xs text-eva-green-400 transition-colors hover:bg-eva-green-500/20"
        >
          <Check className="h-3 w-3" />
          Accept
        </button>
        <button
          type="button"
          onClick={handleRevise}
          className="flex items-center gap-1 rounded border border-terminal-600 px-2 py-1 text-xs text-terminal-400 transition-colors hover:border-terminal-500 hover:text-terminal-200"
        >
          <RotateCcw className="h-3 w-3" />
          Revise
        </button>
      </div>
    </div>
  )
}
