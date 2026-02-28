import { useState } from 'react'
import { ChevronDown, ChevronRight, Loader2 } from 'lucide-react'
import { useProgramChangesets, useAcceptAll, useRejectAll } from '../../api/hooks'
import { useUiStore } from '../../store/uiStore'
import { DiffViewer } from '../editor/DiffViewer'
import type { CodeChangeset } from '../../types'

// ---------------------------------------------------------------------------
// Status badge config
// ---------------------------------------------------------------------------
const CHANGESET_STATUS: Record<
  CodeChangeset['status'],
  { label: string; className: string }
> = {
  pending:  { label: 'pending',  className: 'bg-terminal-700 text-terminal-300' },
  applied:  { label: 'applied',  className: 'bg-eva-green-900 text-eva-green-400' },
  rejected: { label: 'rejected', className: 'bg-nerv-red-900 text-nerv-red-400' },
}

// ---------------------------------------------------------------------------
// ChangesetGroup
// ---------------------------------------------------------------------------
interface ChangesetGroupProps {
  changeset: CodeChangeset
}

function ChangesetGroup({ changeset }: ChangesetGroupProps) {
  const [expanded, setExpanded] = useState(true)

  const acceptAll = useAcceptAll()
  const rejectAll = useRejectAll()

  const isPending = changeset.status === 'pending'
  const isAccepting = acceptAll.isPending
  const isRejecting = rejectAll.isPending

  const status = CHANGESET_STATUS[changeset.status]
  const fileCount = changeset.files.length

  const formattedTime = (() => {
    try {
      return new Date(changeset.createdAt).toLocaleString(undefined, {
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit',
        hour12: false,
      })
    } catch {
      return changeset.createdAt
    }
  })()

  return (
    <div className="border border-terminal-600 bg-terminal-850">
      {/* Group header */}
      <div className="flex h-10 flex-shrink-0 items-center gap-2 px-3 border-b border-terminal-600 bg-terminal-800">
        <button
          onClick={() => setExpanded((v) => !v)}
          className="flex items-center gap-2 flex-1 min-w-0 hover:opacity-80 transition-opacity"
          aria-expanded={expanded}
        >
          {expanded
            ? <ChevronDown size={14} className="flex-shrink-0 text-terminal-400" />
            : <ChevronRight size={14} className="flex-shrink-0 text-terminal-400" />
          }

          {/* Run ID */}
          <span className="font-mono text-xs text-terminal-300 flex-shrink-0">
            run:{changeset.runId.slice(0, 8)}
          </span>

          {/* Timestamp */}
          <span className="text-xs text-terminal-400 flex-shrink-0">{formattedTime}</span>

          {/* File count */}
          <span className="flex-shrink-0 rounded bg-terminal-700 px-1.5 py-0.5 font-mono text-[10px] text-terminal-300">
            {fileCount} {fileCount === 1 ? 'file' : 'files'}
          </span>

          {/* Status badge */}
          <span
            className={`flex-shrink-0 rounded px-2 py-0.5 font-display text-[10px] uppercase tracking-widest ${status.className}`}
          >
            {status.label}
          </span>
        </button>

        {/* Accept All / Reject All — only for pending changesets */}
        {isPending && (
          <div className="flex flex-shrink-0 items-center gap-1.5">
            <button
              onClick={() => void acceptAll.mutate(changeset.id)}
              disabled={isAccepting || isRejecting}
              className={[
                'flex items-center gap-1 rounded px-2.5 py-1 font-display text-[10px] uppercase tracking-widest transition-colors',
                !isAccepting && !isRejecting
                  ? 'bg-eva-green-800 text-eva-green-300 hover:bg-eva-green-700'
                  : 'cursor-not-allowed bg-terminal-700 text-terminal-500',
              ].join(' ')}
            >
              {isAccepting && <Loader2 size={10} className="animate-spin" />}
              Accept All
            </button>

            <button
              onClick={() => void rejectAll.mutate(changeset.id)}
              disabled={isAccepting || isRejecting}
              className={[
                'flex items-center gap-1 rounded px-2.5 py-1 font-display text-[10px] uppercase tracking-widest transition-colors',
                !isAccepting && !isRejecting
                  ? 'bg-nerv-red-900 text-nerv-red-300 hover:bg-nerv-red-800'
                  : 'cursor-not-allowed bg-terminal-700 text-terminal-500',
              ].join(' ')}
            >
              {isRejecting && <Loader2 size={10} className="animate-spin" />}
              Reject All
            </button>
          </div>
        )}
      </div>

      {/* File list */}
      {expanded && (
        <div className="flex flex-col gap-px bg-terminal-700">
          {changeset.files.map((file) => (
            <DiffViewer
              key={file.id}
              file={file}
              changesetId={changeset.id}
              defaultExpanded={false}
            />
          ))}
        </div>
      )}
    </div>
  )
}

// ---------------------------------------------------------------------------
// ChangesPanel
// ---------------------------------------------------------------------------
export function ChangesPanel() {
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const { data: changesets, isLoading, isError } = useProgramChangesets(selectedProgramId)

  if (!selectedProgramId) {
    return (
      <div className="flex h-full w-full items-center justify-center">
        <p className="text-xs text-terminal-400">No program selected</p>
      </div>
    )
  }

  if (isLoading) {
    return (
      <div className="flex h-full w-full items-center justify-center gap-2">
        <Loader2 size={14} className="animate-spin text-terminal-400" />
        <p className="text-xs text-terminal-400">Loading changesets…</p>
      </div>
    )
  }

  if (isError) {
    return (
      <div className="flex h-full w-full items-center justify-center">
        <p className="text-xs text-nerv-red-400">Failed to load changesets</p>
      </div>
    )
  }

  const sorted = [...(changesets ?? [])].sort(
    (a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime(),
  )

  if (sorted.length === 0) {
    return (
      <div className="flex h-full w-full flex-col items-center justify-center gap-1">
        <p className="text-xs text-terminal-300">No pending changes</p>
        <p className="text-[11px] text-terminal-500">
          Agent-proposed file changes will appear here after a run
        </p>
      </div>
    )
  }

  return (
    <div className="flex h-full w-full flex-col overflow-auto">
      <div className="flex flex-col gap-2 p-3">
        {sorted.map((changeset) => (
          <ChangesetGroup key={changeset.id} changeset={changeset} />
        ))}
      </div>
    </div>
  )
}
