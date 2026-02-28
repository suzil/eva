import { useState, useEffect } from 'react'
import {
  Activity,
  AlertCircle,
  CheckCircle2,
  ChevronDown,
  ChevronRight,
  Clock,
  Loader2,
  Pause,
  SkipForward,
  XCircle,
} from 'lucide-react'
import { fetchRunDetail } from '../../api/client'
import { useCanvasStore } from '../../store/canvasStore'
import type { RunDetail, RunState, StepState } from '../../types'

// ---------------------------------------------------------------------------
// State configs (local copies — avoids re-exporting from RunsPanel/StepInspector)
// ---------------------------------------------------------------------------

const RUN_STATE_CONFIG: Record<
  RunState,
  { label: string; icon: React.ReactNode; className: string }
> = {
  completed: {
    label: 'Completed',
    icon: <CheckCircle2 size={11} />,
    className: 'bg-eva-green-900 text-eva-green-400 border-eva-green-800',
  },
  running: {
    label: 'Running',
    icon: <Loader2 size={11} className="animate-spin" />,
    className: 'bg-magi-blue-900 text-magi-blue-400 border-magi-blue-800',
  },
  failed: {
    label: 'Failed',
    icon: <AlertCircle size={11} />,
    className: 'bg-nerv-red-900 text-nerv-red-400 border-nerv-red-800',
  },
  waiting: {
    label: 'Waiting',
    icon: <Pause size={11} />,
    className: 'bg-warn-amber-900 text-warn-amber-400 border-warn-amber-800',
  },
  pending: {
    label: 'Pending',
    icon: <Clock size={11} />,
    className: 'bg-terminal-800 text-terminal-400 border-terminal-600',
  },
  canceled: {
    label: 'Canceled',
    icon: <XCircle size={11} />,
    className: 'bg-terminal-800 text-terminal-500 border-terminal-600',
  },
}

const STEP_STATE_CONFIG: Record<
  StepState,
  { label: string; icon: React.ReactNode; className: string }
> = {
  completed: {
    label: 'Done',
    icon: <CheckCircle2 size={10} />,
    className: 'bg-eva-green-900/60 text-eva-green-400 border-eva-green-800',
  },
  running: {
    label: 'Running',
    icon: <Loader2 size={10} className="animate-spin" />,
    className: 'bg-magi-blue-900/60 text-magi-blue-400 border-magi-blue-800',
  },
  failed: {
    label: 'Failed',
    icon: <AlertCircle size={10} />,
    className: 'bg-nerv-red-900/60 text-nerv-red-400 border-nerv-red-800',
  },
  waiting: {
    label: 'Waiting',
    icon: <Pause size={10} />,
    className: 'bg-warn-amber-900/60 text-warn-amber-400 border-warn-amber-800',
  },
  pending: {
    label: 'Pending',
    icon: <Clock size={10} />,
    className: 'bg-terminal-800 text-terminal-400 border-terminal-600',
  },
  skipped: {
    label: 'Skipped',
    icon: <SkipForward size={10} />,
    className: 'bg-terminal-800 text-terminal-500 border-terminal-600',
  },
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function formatDuration(startedAt?: string, finishedAt?: string): string | null {
  if (!startedAt) return null
  const start = new Date(startedAt).getTime()
  const end = finishedAt ? new Date(finishedAt).getTime() : Date.now()
  const ms = end - start
  if (ms < 1000) return `${ms}ms`
  if (ms < 60_000) return `${(ms / 1000).toFixed(1)}s`
  const min = Math.floor(ms / 60_000)
  const sec = Math.floor((ms % 60_000) / 1000)
  return `${min}m ${sec}s`
}

function shortId(id: string): string {
  return id.slice(0, 8)
}

// ---------------------------------------------------------------------------
// Sub-components
// ---------------------------------------------------------------------------

function RunStateBadge({ state }: { state: RunState }) {
  const cfg = RUN_STATE_CONFIG[state]
  return (
    <span
      className={`inline-flex items-center gap-1 rounded border px-1.5 py-px text-[10px] font-medium ${cfg.className}`}
    >
      {cfg.icon}
      {cfg.label}
    </span>
  )
}

function StepStateBadge({ state }: { state: StepState }) {
  const cfg = STEP_STATE_CONFIG[state]
  return (
    <span
      className={`inline-flex items-center gap-0.5 rounded border px-1 py-px text-[9px] font-medium ${cfg.className}`}
    >
      {cfg.icon}
      {cfg.label}
    </span>
  )
}

// ---------------------------------------------------------------------------
// RunDataCard
// ---------------------------------------------------------------------------

interface RunDataCardProps {
  runId: string
  summary: string
  detail?: RunDetail
}

export function RunDataCard({ runId, summary, detail: detailProp }: RunDataCardProps) {
  const [expanded, setExpanded] = useState(false)
  const [loadedDetail, setLoadedDetail] = useState<RunDetail | null>(detailProp ?? null)
  const [loading, setLoading] = useState(false)

  const nodes = useCanvasStore((s) => s.nodes)

  // Sync if detail prop is provided after initial render (e.g., appended by ActionConfirmCard)
  useEffect(() => {
    if (detailProp && !loadedDetail) {
      setLoadedDetail(detailProp)
    }
  }, [detailProp]) // eslint-disable-line react-hooks/exhaustive-deps

  async function handleToggle() {
    if (!expanded && !loadedDetail) {
      setLoading(true)
      try {
        const detail = await fetchRunDetail(runId)
        setLoadedDetail(detail)
      } catch {
        // show empty step list if fetch fails
      } finally {
        setLoading(false)
      }
    }
    setExpanded((v) => !v)
  }

  const run = loadedDetail?.run
  const steps = loadedDetail?.steps ?? []
  const duration = run ? formatDuration(run.startedAt, run.finishedAt) : null
  const completedSteps = steps.filter((s) => s.state === 'completed').length
  const hasSteps = steps.length > 0 || !loadedDetail

  function nodeLabel(nodeId: string): string {
    const node = nodes.find((n) => n.id === nodeId)
    return node?.data.label ?? shortId(nodeId)
  }

  return (
    <div className="mx-3 rounded border border-terminal-600 bg-terminal-900">
      {/* Header */}
      <div className="flex items-center gap-1.5 border-b border-terminal-700 px-3 py-2 text-xs font-display uppercase tracking-widest text-magi-blue-400">
        <Activity className="h-3.5 w-3.5" />
        Run Data
      </div>

      {/* Summary row */}
      <div className="px-3 pt-2 pb-1">
        <p className="text-xs text-terminal-200 whitespace-pre-wrap">{summary}</p>
      </div>

      {/* Run meta row */}
      {run && (
        <div className="flex flex-wrap items-center gap-2 px-3 pb-2">
          <RunStateBadge state={run.state} />
          {duration && (
            <span className="flex items-center gap-1 text-[10px] text-terminal-400">
              <Clock size={10} />
              {duration}
            </span>
          )}
          {steps.length > 0 && (
            <span className="text-[10px] text-terminal-400">
              {completedSteps}/{steps.length} steps
            </span>
          )}
        </div>
      )}

      {/* Expand toggle */}
      {hasSteps && (
        <button
          type="button"
          onClick={handleToggle}
          className="flex w-full items-center gap-1 border-t border-terminal-700 px-3 py-1.5 text-[10px] text-terminal-400 transition-colors hover:text-terminal-200"
        >
          {loading ? (
            <Loader2 size={10} className="animate-spin" />
          ) : expanded ? (
            <ChevronDown size={10} />
          ) : (
            <ChevronRight size={10} />
          )}
          {expanded ? 'Hide steps' : 'Show steps'}
        </button>
      )}

      {/* Step list */}
      {expanded && loadedDetail && steps.length > 0 && (
        <div className="mx-3 mb-2 space-y-0.5 rounded border border-terminal-700 bg-terminal-950 px-2.5 py-2">
          {steps.map((step) => (
            <div key={step.id} className="space-y-0.5">
              <div className="flex items-center gap-2 font-mono text-[11px]">
                <StepStateBadge state={step.state} />
                <span className="text-terminal-300 truncate">{nodeLabel(step.nodeId)}</span>
              </div>
              {step.error && (
                <p className="pl-1 text-[10px] text-nerv-red-400 whitespace-pre-wrap">
                  {step.error}
                </p>
              )}
            </div>
          ))}
        </div>
      )}

      {expanded && loadedDetail && steps.length === 0 && (
        <p className="px-3 pb-2 text-[10px] text-terminal-500">No steps recorded.</p>
      )}
    </div>
  )
}
