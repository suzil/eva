import { useState } from 'react'
import {
  AlertCircle,
  Check,
  CheckCircle,
  Play,
  Pause,
  PlayCircle,
  Rocket,
  X,
  XCircle,
} from 'lucide-react'
import {
  deployProgram,
  pauseProgram,
  resumeProgram,
  createRun,
  fetchProgram,
  fetchRuns,
  fetchRunDetail,
} from '../../api/client'
import { useUiStore } from '../../store/uiStore'
import type { RunDetail } from '../../types'

// ---------------------------------------------------------------------------
// Operation metadata
// ---------------------------------------------------------------------------

const OPERATION_META: Record<
  string,
  { label: string; icon: React.ReactNode; confirmLabel: string }
> = {
  deploy: {
    label: 'Deploy',
    icon: <Rocket className="h-3.5 w-3.5" />,
    confirmLabel: 'Deploy',
  },
  run: {
    label: 'Run',
    icon: <Play className="h-3.5 w-3.5" />,
    confirmLabel: 'Run Now',
  },
  pause: {
    label: 'Pause',
    icon: <Pause className="h-3.5 w-3.5" />,
    confirmLabel: 'Pause',
  },
  resume: {
    label: 'Resume',
    icon: <PlayCircle className="h-3.5 w-3.5" />,
    confirmLabel: 'Resume',
  },
  status: {
    label: 'Status',
    icon: <AlertCircle className="h-3.5 w-3.5" />,
    confirmLabel: 'Fetch Status',
  },
}

const DEFAULT_META = {
  label: 'Confirm',
  icon: <AlertCircle className="h-3.5 w-3.5" />,
  confirmLabel: 'Confirm',
}

// ---------------------------------------------------------------------------
// ActionConfirmCard
// ---------------------------------------------------------------------------

interface ActionConfirmCardProps {
  operation: string
  description: string
  programId: string
}

type Status = 'pending' | 'loading' | 'done' | 'cancelled' | 'error'

export function ActionConfirmCard({
  operation,
  description,
  programId,
}: ActionConfirmCardProps) {
  const [status, setStatus] = useState<Status>('pending')
  const [resultMsg, setResultMsg] = useState<string | null>(null)
  const appendAssistantMessage = useUiStore((s) => s.appendAssistantMessage)

  const meta = OPERATION_META[operation] ?? DEFAULT_META

  async function handleConfirm() {
    if (!programId) {
      setStatus('error')
      setResultMsg('No program selected.')
      return
    }

    setStatus('loading')
    try {
      switch (operation) {
        case 'deploy': {
          await deployProgram(programId)
          setResultMsg('Program deployed successfully.')
          break
        }
        case 'run': {
          const run = await createRun(programId)
          let detail: RunDetail | undefined
          try {
            detail = await fetchRunDetail(run.id)
          } catch {
            // detail fetch is best-effort; card still shows without steps
          }
          appendAssistantMessage(programId, {
            type: 'run_data',
            runId: run.id,
            summary: `Run started — state: ${run.state}`,
            detail,
            timestamp: Date.now(),
          })
          setResultMsg('Run created — see run data card above.')
          break
        }
        case 'pause': {
          await pauseProgram(programId)
          setResultMsg('Program paused.')
          break
        }
        case 'resume': {
          await resumeProgram(programId)
          setResultMsg('Program resumed.')
          break
        }
        case 'status': {
          const [program, runs] = await Promise.all([
            fetchProgram(programId),
            fetchRuns(programId, 1),
          ])
          const latestRun = runs[0]
          let detail: RunDetail | undefined
          if (latestRun) {
            try {
              detail = await fetchRunDetail(latestRun.id)
            } catch {
              // best-effort
            }
          }
          if (latestRun) {
            appendAssistantMessage(programId, {
              type: 'run_data',
              runId: latestRun.id,
              summary: `Program ${program.state} — last run: ${latestRun.state}`,
              detail,
              timestamp: Date.now(),
            })
          }
          setResultMsg(
            latestRun
              ? `Status retrieved — program ${program.state}.`
              : `Program ${program.state} — no runs yet.`,
          )
          break
        }
        default: {
          setResultMsg(`Operation "${operation}" completed.`)
        }
      }
      setStatus('done')
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err)
      setResultMsg(msg)
      setStatus('error')
    }
  }

  function handleCancel() {
    setStatus('cancelled')
  }

  // Settled states — show compact result row
  if (status === 'done') {
    return (
      <div className="mx-3 rounded border border-eva-green-500/30 bg-eva-green-500/5 px-3 py-2">
        <div className="flex items-center gap-1.5 text-xs text-eva-green-500">
          <CheckCircle className="h-3.5 w-3.5 flex-shrink-0" />
          <span className="font-display uppercase tracking-widest">{meta.label}</span>
          {resultMsg && <span className="text-eva-green-400 normal-case font-normal tracking-normal">— {resultMsg}</span>}
        </div>
      </div>
    )
  }

  if (status === 'error') {
    return (
      <div className="mx-3 rounded border border-nerv-red-500/30 bg-nerv-red-500/5 px-3 py-2">
        <div className="flex items-center gap-1.5 text-xs text-nerv-red-400">
          <XCircle className="h-3.5 w-3.5 flex-shrink-0" />
          <span className="font-display uppercase tracking-widest">{meta.label} failed</span>
          {resultMsg && <span className="normal-case font-normal tracking-normal">— {resultMsg}</span>}
        </div>
      </div>
    )
  }

  if (status === 'cancelled') {
    return (
      <div className="mx-3 rounded border border-terminal-700 bg-terminal-900/50 px-3 py-2 opacity-50">
        <div className="flex items-center gap-1.5 text-xs text-terminal-500">
          <X className="h-3.5 w-3.5 flex-shrink-0" />
          <span className="font-display uppercase tracking-widest">{meta.label} — cancelled</span>
        </div>
      </div>
    )
  }

  // Pending / loading state
  return (
    <div className="mx-3 rounded border border-terminal-600 bg-terminal-900">
      {/* Header */}
      <div className="flex items-center gap-1.5 border-b border-terminal-700 px-3 py-2 text-xs font-display uppercase tracking-widest text-warn-amber-400">
        {meta.icon}
        {meta.label}
      </div>

      {/* Description */}
      <div className="px-3 py-2">
        <p className="text-xs text-terminal-200 whitespace-pre-wrap">{description}</p>
      </div>

      {/* Buttons */}
      <div className="flex items-center gap-2 border-t border-terminal-700 px-3 py-2">
        <button
          type="button"
          onClick={handleConfirm}
          disabled={status === 'loading'}
          className="flex items-center gap-1 rounded border border-magi-blue-500/50 bg-magi-blue-500/10 px-2.5 py-1 text-xs text-magi-blue-400 transition-colors hover:bg-magi-blue-500/20 disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {status === 'loading' ? (
            <span className="inline-block h-3 w-3 animate-spin rounded-full border border-magi-blue-400 border-t-transparent" />
          ) : (
            <Check className="h-3 w-3" />
          )}
          {status === 'loading' ? 'Running…' : meta.confirmLabel}
        </button>
        <button
          type="button"
          onClick={handleCancel}
          disabled={status === 'loading'}
          className="flex items-center gap-1 px-2.5 py-1 text-xs text-terminal-400 transition-colors hover:text-terminal-200 disabled:opacity-50 disabled:cursor-not-allowed"
        >
          <X className="h-3 w-3" />
          Cancel
        </button>
      </div>
    </div>
  )
}
