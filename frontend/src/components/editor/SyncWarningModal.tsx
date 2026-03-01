import { useEffect, useRef } from 'react'
import { AlertTriangle } from 'lucide-react'
import type { SpecSyncState } from '../../store/uiStore'
import { useFocusTrap } from '../../hooks/useFocusTrap'

interface SyncWarningModalProps {
  onReplace: () => void
  onKeep: () => void
}

/**
 * Backdrop modal shown when specSyncState === 'conflict' — the graph was
 * modified on the GRAPH tab while the SPEC tab had unsaved edits.
 *
 * Replace Graph: discard YAML edits, reload from the current backend graph.
 * Keep My Edits: preserve local YAML, switch sync source to yaml_source.
 */
export function SyncWarningModal({ onReplace, onKeep }: SyncWarningModalProps) {
  const dialogRef = useRef<HTMLDivElement>(null)

  useFocusTrap(dialogRef, true)

  // Close (keep edits) on Escape — it's the safe/non-destructive default
  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onKeep()
    }
    document.addEventListener('keydown', handler)
    return () => document.removeEventListener('keydown', handler)
  }, [onKeep])

  return (
    <div
      ref={dialogRef}
      role="dialog"
      aria-modal="true"
      aria-labelledby="sync-warning-title"
      aria-describedby="sync-warning-desc"
      className="absolute inset-0 z-50 flex items-center justify-center bg-void-900/80 backdrop-blur-sm"
    >
      <div className="w-full max-w-sm rounded border border-warn-amber-500/40 bg-terminal-800 p-6 shadow-xl">
        <div className="mb-4 flex items-start gap-3">
          <AlertTriangle className="mt-0.5 h-5 w-5 flex-shrink-0 text-warn-amber-400" aria-hidden="true" />
          <div>
            <h2
              id="sync-warning-title"
              className="font-display text-sm uppercase tracking-widest text-terminal-50"
            >
              Graph modified
            </h2>
            <p id="sync-warning-desc" className="mt-1 text-xs text-terminal-300">
              The graph was edited while the Spec tab was open. Do you want to
              replace your YAML edits with the current graph, or keep your YAML
              and discard the graph changes?
            </p>
          </div>
        </div>

        <div className="flex gap-2">
          <button
            onClick={onReplace}
            className="flex-1 rounded border border-warn-amber-500/50 bg-warn-amber-500/10 px-3 py-2 font-display text-xs uppercase tracking-widest text-warn-amber-300 transition-colors hover:bg-warn-amber-500/20"
          >
            Replace Graph
          </button>
          <button
            onClick={onKeep}
            className="flex-1 rounded border border-terminal-500 bg-terminal-700 px-3 py-2 font-display text-xs uppercase tracking-widest text-terminal-200 transition-colors hover:bg-terminal-600"
          >
            Keep My Edits
          </button>
        </div>
      </div>
    </div>
  )
}

/** Type guard to check if sync state is in conflict. */
export function isConflict(state: SpecSyncState): boolean {
  return state === 'conflict'
}
