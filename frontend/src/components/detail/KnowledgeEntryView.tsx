import { useState } from 'react'
import { X, RotateCcw, Trash2, Loader2, Pencil, Check } from 'lucide-react'
import { useUiStore } from '../../store/uiStore'
import {
  useKnowledgeEntry,
  usePatchKnowledgeEntry,
  useDeleteKnowledgeEntry,
  useResetKnowledgeEntry,
} from '../../api/hooks'

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function formatDate(iso: string): string {
  try {
    return new Date(iso).toLocaleString(undefined, {
      month: 'short',
      day: 'numeric',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
      hour12: false,
    })
  } catch {
    return iso
  }
}

function confidenceDotClass(confidence: number): string {
  if (confidence >= 0.9) return 'bg-eva-green-400'
  if (confidence >= 0.7) return 'bg-warn-amber-400'
  return 'bg-nerv-red-400'
}

const SOURCE_BADGE: Record<string, string> = {
  codebase: 'bg-at-field-900 text-at-field-300',
  linear:   'bg-eva-green-900 text-eva-green-400',
  manual:   'bg-terminal-700 text-terminal-300',
  github:   'bg-terminal-700 text-terminal-400',
  http:     'bg-terminal-700 text-terminal-400',
}

function sourceBadgeClass(sourceType: string): string {
  return SOURCE_BADGE[sourceType] ?? 'bg-terminal-700 text-terminal-400'
}

const labelClass = 'mb-1 font-display text-[10px] uppercase tracking-widest text-terminal-400'
const metaValueClass = 'text-[11px] text-terminal-200'

// ---------------------------------------------------------------------------
// KnowledgeEntryView
// ---------------------------------------------------------------------------

interface Props {
  entryId: string
}

export function KnowledgeEntryView({ entryId }: Props) {
  const setSelectedKnowledgeEntryId = useUiStore((s) => s.setSelectedKnowledgeEntryId)

  const { data: entry, isLoading, isError } = useKnowledgeEntry(entryId)
  const patch = usePatchKnowledgeEntry(entryId)
  const deleteEntry = useDeleteKnowledgeEntry()
  const reset = useResetKnowledgeEntry(entryId)

  const [isEditing, setIsEditing] = useState(false)
  const [titleDraft, setTitleDraft] = useState('')
  const [contentDraft, setContentDraft] = useState('')

  if (isLoading) {
    return (
      <div className="flex flex-1 items-center justify-center gap-2">
        <Loader2 size={14} className="animate-spin text-terminal-400" />
        <span className="text-[11px] text-terminal-400">Loading…</span>
      </div>
    )
  }

  if (isError || !entry) {
    return (
      <div className="flex flex-1 items-center justify-center p-4">
        <p className="text-center text-[11px] text-nerv-red-400">Failed to load entry</p>
      </div>
    )
  }

  const canReset = entry.isEdited && entry.sourceType !== 'manual'

  const handleStartEdit = () => {
    setTitleDraft(entry.title)
    setContentDraft(entry.content)
    setIsEditing(true)
  }

  const handleSaveEdit = () => {
    patch.mutate(
      { title: titleDraft.trim() || undefined, content: contentDraft || undefined },
      { onSuccess: () => setIsEditing(false) },
    )
  }

  const handleCancelEdit = () => {
    setIsEditing(false)
  }

  const handleDelete = () => {
    deleteEntry.mutate(entryId, {
      onSuccess: () => setSelectedKnowledgeEntryId(null),
    })
  }

  const handleReset = () => {
    reset.mutate()
  }

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Header */}
      <div className="flex shrink-0 items-center gap-2 border-b border-terminal-500 px-3 py-2">
        <span
          className={`flex-shrink-0 rounded px-1.5 py-0.5 font-display text-[9px] uppercase tracking-widest ${sourceBadgeClass(entry.sourceType)}`}
        >
          {entry.sourceType}
        </span>

        {isEditing ? (
          <input
            value={titleDraft}
            onChange={(e) => setTitleDraft(e.target.value)}
            className="min-w-0 flex-1 rounded bg-terminal-700/60 px-1.5 py-0.5 text-sm font-semibold text-terminal-50 outline-none ring-1 ring-at-field-500/40 focus:ring-at-field-500"
            autoFocus
          />
        ) : (
          <span className="min-w-0 flex-1 truncate text-sm font-semibold text-terminal-50">
            {entry.title}
          </span>
        )}

        {/* Action buttons */}
        <div className="flex flex-shrink-0 items-center gap-1">
          {isEditing ? (
            <>
              <button
                onClick={handleSaveEdit}
                disabled={patch.isPending}
                title="Save"
                className="flex h-6 w-6 items-center justify-center rounded border border-eva-green-700 bg-eva-green-900/60 text-eva-green-400 transition-colors hover:bg-eva-green-800 disabled:opacity-40"
              >
                {patch.isPending
                  ? <Loader2 size={11} className="animate-spin" />
                  : <Check size={11} />
                }
              </button>
              <button
                onClick={handleCancelEdit}
                title="Cancel"
                className="flex h-6 w-6 items-center justify-center rounded border border-terminal-500 bg-terminal-700 text-terminal-400 transition-colors hover:text-terminal-100"
              >
                <X size={11} />
              </button>
            </>
          ) : (
            <>
              <button
                onClick={handleStartEdit}
                title="Edit"
                className="flex h-6 w-6 items-center justify-center rounded border border-terminal-500 bg-terminal-700 text-terminal-400 transition-colors hover:text-terminal-100"
              >
                <Pencil size={11} />
              </button>
              {canReset && (
                <button
                  onClick={handleReset}
                  disabled={reset.isPending}
                  title="Reset to auto-generated content"
                  className="flex h-6 w-6 items-center justify-center rounded border border-warn-amber-700 bg-warn-amber-950/40 text-warn-amber-400 transition-colors hover:bg-warn-amber-900 disabled:opacity-40"
                >
                  {reset.isPending
                    ? <Loader2 size={11} className="animate-spin" />
                    : <RotateCcw size={11} />
                  }
                </button>
              )}
              <button
                onClick={handleDelete}
                disabled={deleteEntry.isPending}
                title="Delete entry"
                className="flex h-6 w-6 items-center justify-center rounded border border-nerv-red-800 bg-nerv-red-950/40 text-nerv-red-400 transition-colors hover:bg-nerv-red-900 disabled:opacity-40"
              >
                {deleteEntry.isPending
                  ? <Loader2 size={11} className="animate-spin" />
                  : <Trash2 size={11} />
                }
              </button>
              <button
                onClick={() => setSelectedKnowledgeEntryId(null)}
                title="Close"
                className="flex h-6 w-6 items-center justify-center rounded border border-terminal-500 bg-terminal-700 text-terminal-400 transition-colors hover:text-terminal-100"
              >
                <X size={11} />
              </button>
            </>
          )}
        </div>
      </div>

      {/* Body */}
      <div className="flex-1 overflow-y-auto p-3 space-y-4">
        {/* Content */}
        <div>
          <p className={labelClass}>Content</p>
          {isEditing ? (
            <textarea
              value={contentDraft}
              onChange={(e) => setContentDraft(e.target.value)}
              rows={8}
              className="w-full rounded border border-terminal-500 bg-terminal-900 px-2 py-1.5 font-mono text-[11px] leading-relaxed text-terminal-100 outline-none focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 resize-y"
            />
          ) : (
            <pre className="whitespace-pre-wrap break-words rounded border border-terminal-600 bg-terminal-900 px-2 py-1.5 font-mono text-[11px] leading-relaxed text-terminal-200">
              {entry.content}
            </pre>
          )}
        </div>

        {/* Metadata grid */}
        <div className="grid grid-cols-2 gap-x-3 gap-y-2">
          <div>
            <p className={labelClass}>Category</p>
            <p className={metaValueClass}>{entry.category}</p>
          </div>

          <div>
            <p className={labelClass}>Confidence</p>
            <div className="flex items-center gap-1.5">
              <span className={`h-2 w-2 flex-shrink-0 rounded-full ${confidenceDotClass(entry.confidence)}`} />
              <span className={metaValueClass}>{Math.round(entry.confidence * 100)}%</span>
            </div>
          </div>

          <div>
            <p className={labelClass}>Scanned at</p>
            <p className={metaValueClass}>{formatDate(entry.scannedAt)}</p>
          </div>

          <div>
            <p className={labelClass}>Updated at</p>
            <p className={metaValueClass}>{formatDate(entry.updatedAt)}</p>
          </div>

          {entry.isEdited && (
            <div className="col-span-2">
              <span className="rounded border border-warn-amber-700 bg-warn-amber-950/40 px-1.5 py-0.5 font-display text-[9px] uppercase tracking-widest text-warn-amber-400">
                Manually edited
              </span>
            </div>
          )}
        </div>

        {/* Save error */}
        {patch.isError && (
          <p className="rounded border border-nerv-red-700 bg-nerv-red-950/40 px-2 py-1 text-[10px] text-nerv-red-400">
            Save failed — {(patch.error as Error).message}
          </p>
        )}
      </div>
    </div>
  )
}
