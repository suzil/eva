import { useState } from 'react'
import {
  AlertCircle,
  CheckCircle2,
  Clock,
  History,
  Loader2,
  Pause,
  XCircle,
} from 'lucide-react'
import { useRuns } from '../../api/hooks'
import { useUiStore } from '../../store/uiStore'
import type { Run, RunState } from '../../types'

// ---------------------------------------------------------------------------
// State config — mirrors StepInspector.tsx STATE_CONFIG for visual consistency
// ---------------------------------------------------------------------------

const RUN_STATE_CONFIG: Record<
  RunState,
  { label: string; icon: React.ReactNode; className: string; dotClass: string }
> = {
  completed: {
    label: 'Completed',
    icon: <CheckCircle2 size={11} />,
    className: 'bg-green-900/60 text-green-400 border-green-800',
    dotClass: 'bg-green-500',
  },
  running: {
    label: 'Running',
    icon: <Loader2 size={11} className="animate-spin" />,
    className: 'bg-blue-900/60 text-blue-400 border-blue-800',
    dotClass: 'bg-blue-500',
  },
  failed: {
    label: 'Failed',
    icon: <AlertCircle size={11} />,
    className: 'bg-red-900/60 text-red-400 border-red-800',
    dotClass: 'bg-red-500',
  },
  waiting: {
    label: 'Waiting',
    icon: <Pause size={11} />,
    className: 'bg-amber-900/60 text-amber-400 border-amber-800',
    dotClass: 'bg-amber-500',
  },
  pending: {
    label: 'Pending',
    icon: <Clock size={11} />,
    className: 'bg-gray-800 text-gray-500 border-gray-700',
    dotClass: 'bg-gray-500',
  },
  canceled: {
    label: 'Canceled',
    icon: <XCircle size={11} />,
    className: 'bg-gray-800 text-gray-600 border-gray-700',
    dotClass: 'bg-gray-600',
  },
}

type FilterState = 'all' | 'running' | 'completed' | 'failed'

const FILTERS: { key: FilterState; label: string }[] = [
  { key: 'all', label: 'All' },
  { key: 'running', label: 'Running' },
  { key: 'completed', label: 'Done' },
  { key: 'failed', label: 'Failed' },
]

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function formatDate(iso: string | undefined): string {
  if (!iso) return 'Pending'
  return new Date(iso).toLocaleString(undefined, {
    month: 'short',
    day: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
  })
}

function formatDuration(startedAt: string | undefined, finishedAt: string | undefined): string | null {
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

// ---------------------------------------------------------------------------
// RunStateBadge
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

// ---------------------------------------------------------------------------
// RunRow
// ---------------------------------------------------------------------------

function RunRow({
  run,
  isActive,
  isInspected,
  onClick,
}: {
  run: Run
  isActive: boolean
  isInspected: boolean
  onClick: () => void
}) {
  const duration = formatDuration(run.startedAt, run.finishedAt)

  return (
    <button
      onClick={onClick}
      className={[
        'flex w-full flex-col gap-1.5 px-3 py-2.5 text-left transition-colors',
        isInspected
          ? 'bg-gray-700/60 border-l-2 border-blue-500'
          : 'border-l-2 border-transparent hover:bg-gray-800/60',
      ].join(' ')}
    >
      {/* Top row: state badge + active indicator */}
      <div className="flex items-center gap-2">
        <RunStateBadge state={run.state} />
        {isActive && (
          <span className="flex items-center gap-1 text-[10px] text-blue-400">
            <span className="relative flex h-1.5 w-1.5">
              <span className="absolute inline-flex h-full w-full animate-ping rounded-full bg-blue-400 opacity-75" />
              <span className="relative inline-flex h-1.5 w-1.5 rounded-full bg-blue-500" />
            </span>
            live
          </span>
        )}
      </div>

      {/* Bottom row: timestamp + duration */}
      <div className="flex items-center justify-between text-[10px] text-gray-500">
        <span>{formatDate(run.startedAt)}</span>
        {duration && <span className="text-gray-600">{duration}</span>}
      </div>
    </button>
  )
}

// ---------------------------------------------------------------------------
// RunsPanel
// ---------------------------------------------------------------------------

export function RunsPanel() {
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const activeRunId = useUiStore((s) => s.activeRunId)
  const inspectedRunId = useUiStore((s) => s.inspectedRunId)
  const setInspectedRunId = useUiStore((s) => s.setInspectedRunId)
  const setMode = useUiStore((s) => s.setMode)

  const { data: runs = [], isLoading } = useRuns(selectedProgramId)

  const [filter, setFilter] = useState<FilterState>('all')

  const filteredRuns = filter === 'all'
    ? runs
    : runs.filter((r) => {
        if (filter === 'running') return r.state === 'running' || r.state === 'waiting' || r.state === 'pending'
        if (filter === 'completed') return r.state === 'completed'
        if (filter === 'failed') return r.state === 'failed' || r.state === 'canceled'
        return true
      })

  const handleSelectRun = (run: Run) => {
    setInspectedRunId(run.id)
    setMode('operate')
  }

  if (!selectedProgramId) {
    return (
      <div className="flex flex-1 flex-col items-center justify-center gap-2 p-4">
        <History className="h-7 w-7 text-gray-700" />
        <p className="text-center text-[10px] text-gray-600">Select a program to view runs</p>
      </div>
    )
  }

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Filter bar */}
      <div className="flex gap-px border-b border-gray-800 bg-gray-900 px-2 py-1.5">
        {FILTERS.map(({ key, label }) => (
          <button
            key={key}
            onClick={() => setFilter(key)}
            className={[
              'rounded px-2 py-0.5 text-[10px] font-medium transition-colors',
              filter === key
                ? 'bg-gray-700 text-gray-200'
                : 'text-gray-500 hover:text-gray-300',
            ].join(' ')}
          >
            {label}
          </button>
        ))}
      </div>

      {/* Run list */}
      <div className="flex-1 overflow-y-auto">
        {isLoading && (
          <div className="flex items-center justify-center py-8">
            <Loader2 className="h-4 w-4 animate-spin text-gray-600" />
          </div>
        )}

        {!isLoading && filteredRuns.length === 0 && (
          <div className="flex flex-col items-center justify-center gap-2 py-8">
            <History className="h-6 w-6 text-gray-700" />
            <p className="text-center text-[10px] text-gray-600">
              {filter === 'all' ? 'No runs yet' : `No ${filter} runs`}
            </p>
          </div>
        )}

        {!isLoading &&
          filteredRuns.map((run) => (
            <RunRow
              key={run.id}
              run={run}
              isActive={run.id === activeRunId}
              isInspected={run.id === inspectedRunId}
              onClick={() => handleSelectRun(run)}
            />
          ))}
      </div>
    </div>
  )
}
