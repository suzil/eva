import { useState } from 'react'
import { GitMerge, Check, X, Loader2 } from 'lucide-react'
import { useCanvasStore } from '../../store/canvasStore'
import { useSaveGraph } from '../../api/hooks'
import type { GraphDiff } from '../../types'
import { NODE_TYPE_COLORS, NODE_TYPE_LABELS } from '../../constants/nodeConstants'

const DIFF_COLORS = {
  added:        '#33FF5C',  // eva-green-400
  modified:     '#FFCC33',  // warn-amber-400
  removed:      '#FF7A7A',  // nerv-red-300
  nodeFallback: '#4F5070',  // terminal-400
} as const

interface GraphDiffCardProps {
  diff: GraphDiff
  summary: string
}

// ---------------------------------------------------------------------------
// Change list rows
// ---------------------------------------------------------------------------

function RemovedNodeRow({ label, nodeTypeName }: { label: string; nodeTypeName: string }) {
  return (
    <div className="flex items-baseline gap-1.5 font-mono text-[11px]">
      <span className="text-nerv-red-500 select-none">-</span>
      <span className="text-terminal-400">Node:</span>
      <span className="text-nerv-red-400">{label}</span>
      <span className="text-terminal-600">({nodeTypeName})</span>
    </div>
  )
}

function ModifiedRow({ label, nodeTypeName }: { label: string; nodeTypeName: string }) {
  return (
    <div className="flex items-baseline gap-1.5 font-mono text-[11px]">
      <span className="text-warn-amber-500 select-none">~</span>
      <span className="text-terminal-400">Node:</span>
      <span className="text-warn-amber-400">{label}</span>
      <span className="text-terminal-600">({nodeTypeName})</span>
    </div>
  )
}

function AddedEdgeRow({ sourceLabel, targetLabel }: { sourceLabel: string; targetLabel: string }) {
  return (
    <div className="flex items-baseline gap-1.5 font-mono text-[11px]">
      <span className="text-eva-green-500 select-none">+</span>
      <span className="text-terminal-400">Edge:</span>
      <span className="text-eva-green-400">
        {sourceLabel} → {targetLabel}
      </span>
    </div>
  )
}

function RemovedEdgeRow({ sourceLabel, targetLabel }: { sourceLabel: string; targetLabel: string }) {
  return (
    <div className="flex items-baseline gap-1.5 font-mono text-[11px]">
      <span className="text-nerv-red-500 select-none">-</span>
      <span className="text-terminal-400">Edge:</span>
      <span className="text-nerv-red-400">
        {sourceLabel} → {targetLabel}
      </span>
    </div>
  )
}

// ---------------------------------------------------------------------------
// Count badge
// ---------------------------------------------------------------------------

function CountBadge({ count, color }: { count: number; color: string }) {
  if (count === 0) return null
  return (
    <span
      className="rounded px-1.5 py-0.5 text-[10px] font-mono"
      style={{ backgroundColor: `${color}22`, color, border: `1px solid ${color}44` }}
    >
      {count}
    </span>
  )
}

// ---------------------------------------------------------------------------
// Main card
// ---------------------------------------------------------------------------

export function GraphDiffCard({ diff, summary }: GraphDiffCardProps) {
  const [accepted, setAccepted] = useState(false)
  const [rejected, setRejected] = useState(false)

  const applyGraphDiff = useCanvasStore((s) => s.applyGraphDiff)
  const buildGraph = useCanvasStore((s) => s.buildGraph)
  const currentProgramId = useCanvasStore((s) => s.currentProgramId)
  const canvasNodes = useCanvasStore((s) => s.nodes)
  const canvasEdges = useCanvasStore((s) => s.edges)

  const saveMutation = useSaveGraph(currentProgramId ?? '')

  // Look up label + type for a node id (for removed/modified nodes which are
  // still on canvas at render time)
  function lookupNode(nodeId: string) {
    const n = canvasNodes.find((cn) => cn.id === nodeId)
    const label = n?.data.label ?? nodeId
    const nodeType = n?.data.nodeType.type ?? ''
    const nodeTypeName = NODE_TYPE_LABELS[nodeType] ?? nodeType
    return { label, nodeTypeName }
  }

  function lookupEdge(edgeId: string) {
    const e = canvasEdges.find((ce) => ce.id === edgeId)
    const srcNode = canvasNodes.find((cn) => cn.id === e?.source)
    const tgtNode = canvasNodes.find((cn) => cn.id === e?.target)
    return {
      sourceLabel: srcNode?.data.label ?? e?.source ?? edgeId,
      targetLabel: tgtNode?.data.label ?? e?.target ?? edgeId,
    }
  }

  const totalChanges =
    diff.addedNodes.length +
    diff.removedNodeIds.length +
    diff.modifiedNodes.length +
    diff.addedEdges.length +
    diff.removedEdgeIds.length

  function handleAccept() {
    applyGraphDiff(diff)
    setAccepted(true)
    // Persist to backend so the program is immediately runnable
    if (currentProgramId) {
      saveMutation.mutate(buildGraph())
    }
  }

  function handleReject() {
    setRejected(true)
  }

  const isSaving = saveMutation.isPending

  return (
    <div
      className={[
        'mx-3 rounded border bg-terminal-900 transition-colors',
        accepted ? 'border-eva-green-500/40' : 'border-terminal-600',
      ].join(' ')}
    >
      {/* Header */}
      <div
        className={[
          'flex items-center gap-1.5 border-b px-3 py-2 text-xs font-display uppercase tracking-widest transition-colors',
          accepted
            ? 'border-eva-green-500/30 text-eva-green-400'
            : 'border-terminal-700 text-magi-blue-400',
        ].join(' ')}
      >
        {accepted ? <Check className="h-3.5 w-3.5" /> : <GitMerge className="h-3.5 w-3.5" />}
        {accepted ? `Applied — ${totalChanges} change${totalChanges !== 1 ? 's' : ''}` : 'Program Diff'}
        {!accepted && (
          <div className="ml-auto flex items-center gap-1">
            <CountBadge count={diff.addedNodes.length + diff.addedEdges.length} color={DIFF_COLORS.added} />
            <CountBadge count={diff.modifiedNodes.length} color={DIFF_COLORS.modified} />
            <CountBadge count={diff.removedNodeIds.length + diff.removedEdgeIds.length} color={DIFF_COLORS.removed} />
          </div>
        )}
      </div>

      {/* Summary */}
      <div className="px-3 pt-2 pb-1">
        <p className="text-xs text-terminal-200 whitespace-pre-wrap">{summary}</p>
      </div>

      {/* Change list */}
      {totalChanges > 0 && (
        <div className="mx-3 mb-2 mt-1 space-y-0.5 rounded border border-terminal-700 bg-terminal-950 px-2.5 py-2">
          {diff.addedNodes.map((n) => {
            const nodeTypeName = NODE_TYPE_LABELS[n.type.type] ?? n.type.type
            const color = NODE_TYPE_COLORS[n.type.type] ?? DIFF_COLORS.nodeFallback
            return (
              <div key={n.id} className="flex items-baseline gap-1.5 font-mono text-[11px]">
                <span className="text-eva-green-500 select-none">+</span>
                <span className="text-terminal-400">Node:</span>
                <span className="text-eva-green-400">{n.label}</span>
                <span
                  className="rounded px-1 text-[9px]"
                  style={{ backgroundColor: `${color}22`, color, border: `1px solid ${color}44` }}
                >
                  {nodeTypeName}
                </span>
              </div>
            )
          })}

          {diff.modifiedNodes.map((m) => {
            const { label, nodeTypeName } = lookupNode(m.nodeId)
            return <ModifiedRow key={m.nodeId} label={label} nodeTypeName={nodeTypeName} />
          })}

          {diff.removedNodeIds.map((id) => {
            const { label, nodeTypeName } = lookupNode(id)
            return <RemovedNodeRow key={id} label={label} nodeTypeName={nodeTypeName} />
          })}

          {diff.addedEdges.map((e) => {
            // Source/target may be a newly-added node (domain Node with .label)
            // or an existing canvas node (react-flow Node<EvaNodeData> with .data.label)
            const addedSrc = diff.addedNodes.find((n) => n.id === e.sourceNode)
            const addedTgt = diff.addedNodes.find((n) => n.id === e.targetNode)
            const canvasSrc = canvasNodes.find((n) => n.id === e.sourceNode)
            const canvasTgt = canvasNodes.find((n) => n.id === e.targetNode)
            const sourceLabel = addedSrc?.label ?? canvasSrc?.data.label ?? e.sourceNode
            const targetLabel = addedTgt?.label ?? canvasTgt?.data.label ?? e.targetNode
            return <AddedEdgeRow key={e.id} sourceLabel={sourceLabel} targetLabel={targetLabel} />
          })}

          {diff.removedEdgeIds.map((id) => {
            const { sourceLabel, targetLabel } = lookupEdge(id)
            return <RemovedEdgeRow key={id} sourceLabel={sourceLabel} targetLabel={targetLabel} />
          })}
        </div>
      )}

      {/* Action bar */}
      <div className="flex items-center gap-2 border-t border-terminal-700 px-3 py-2">
        {accepted ? (
          <div className="flex w-full items-center gap-1.5 text-xs text-eva-green-500">
            {isSaving ? (
              <Loader2 className="h-3.5 w-3.5 animate-spin" />
            ) : (
              <Check className="h-3.5 w-3.5" />
            )}
            <span className="font-display uppercase tracking-widest">
              {isSaving ? 'Saving…' : 'Applied to canvas'}
            </span>
          </div>
        ) : rejected ? (
          <span className="text-xs italic text-terminal-500">Rejected</span>
        ) : (
          <>
            <button
              type="button"
              onClick={handleAccept}
              className="flex items-center gap-1 rounded border border-eva-green-500/40 bg-eva-green-500/10 px-2 py-1 text-xs text-eva-green-400 transition-colors hover:bg-eva-green-500/20"
            >
              <Check className="h-3 w-3" />
              Accept All
            </button>
            <button
              type="button"
              onClick={handleReject}
              className="flex items-center gap-1 rounded border border-terminal-600 px-2 py-1 text-xs text-terminal-400 transition-colors hover:border-nerv-red-500/40 hover:text-nerv-red-400"
            >
              <X className="h-3 w-3" />
              Reject
            </button>
          </>
        )}
      </div>
    </div>
  )
}
