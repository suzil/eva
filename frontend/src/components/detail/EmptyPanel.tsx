import { useEffect, useRef, useState } from 'react'
import { MousePointerClick, Pencil, Check, X } from 'lucide-react'
import { useProgram, usePatchProgram } from '../../api/hooks'
import { useUiStore } from '../../store/uiStore'
import type { ProgramState } from '../../types'

const STATE_LABELS: Record<ProgramState, { label: string; className: string }> = {
  draft:    { label: 'Draft',    className: 'text-terminal-400 border-terminal-500' },
  active:   { label: 'Active',   className: 'text-eva-green-400 border-eva-green-800' },
  paused:   { label: 'Paused',   className: 'text-warn-amber-400 border-warn-amber-800' },
  archived: { label: 'Archived', className: 'text-terminal-500 border-terminal-600' },
}

// ---------------------------------------------------------------------------
// Inline editable field — single-line (title) or multi-line (description)
// ---------------------------------------------------------------------------

interface EditableFieldProps {
  value: string
  placeholder: string
  multiline?: boolean
  className?: string
  onSave: (value: string) => void
}

function EditableField({ value, placeholder, multiline, className, onSave }: EditableFieldProps) {
  const [editing, setEditing] = useState(false)
  const [draft, setDraft] = useState(value)
  const ref = useRef<HTMLInputElement & HTMLTextAreaElement>(null)

  useEffect(() => {
    if (editing) {
      setDraft(value)
      ref.current?.focus()
      if (!multiline) {
        ref.current?.select()
      }
    }
  }, [editing, value, multiline])

  function commit() {
    const trimmed = draft.trim()
    if (trimmed !== value) onSave(trimmed)
    setEditing(false)
  }

  function cancel() {
    setDraft(value)
    setEditing(false)
  }

  function handleKeyDown(e: React.KeyboardEvent) {
    if (e.key === 'Enter' && !multiline) { e.preventDefault(); commit() }
    if (e.key === 'Escape') cancel()
    if (e.key === 'Enter' && multiline && e.metaKey) { e.preventDefault(); commit() }
  }

  if (editing) {
    const sharedClass =
      'w-full rounded bg-terminal-800 px-2 py-1.5 text-terminal-100 outline-none ring-1 ring-at-field-500 placeholder:text-terminal-600'

    return (
      <div className="group relative">
        {multiline ? (
          <textarea
            ref={ref as React.Ref<HTMLTextAreaElement>}
            value={draft}
            placeholder={placeholder}
            onChange={(e) => setDraft(e.target.value)}
            onKeyDown={handleKeyDown}
            rows={4}
            className={`${sharedClass} resize-none text-xs leading-relaxed ${className ?? ''}`}
          />
        ) : (
          <input
            ref={ref as React.Ref<HTMLInputElement>}
            value={draft}
            placeholder={placeholder}
            onChange={(e) => setDraft(e.target.value)}
            onKeyDown={handleKeyDown}
            className={`${sharedClass} text-sm font-semibold ${className ?? ''}`}
          />
        )}
        <div className="mt-1 flex items-center gap-1.5">
          <button
            type="button"
            onClick={commit}
            className="flex items-center gap-1 rounded border border-eva-green-700 bg-eva-green-900/30 px-2 py-0.5 text-[10px] text-eva-green-400 transition-colors hover:bg-eva-green-900/60"
          >
            <Check className="h-2.5 w-2.5" />
            Save
          </button>
          <button
            type="button"
            onClick={cancel}
            className="flex items-center gap-1 rounded border border-terminal-600 px-2 py-0.5 text-[10px] text-terminal-400 transition-colors hover:text-terminal-200"
          >
            <X className="h-2.5 w-2.5" />
            Cancel
          </button>
          {multiline && (
            <span className="ml-auto text-[9px] text-terminal-600">⌘↵ to save</span>
          )}
        </div>
      </div>
    )
  }

  return (
    <div
      className="group/field relative cursor-text"
      onClick={() => setEditing(true)}
      role="button"
      tabIndex={0}
      onKeyDown={(e) => e.key === 'Enter' && setEditing(true)}
    >
      {value ? (
        <div className={`flex items-start gap-1.5 ${className ?? ''}`}>
          <span className="flex-1">{value}</span>
          <Pencil className="mt-0.5 h-3 w-3 shrink-0 text-terminal-600 opacity-0 transition-opacity group-hover/field:opacity-100" />
        </div>
      ) : (
        <div className={`flex items-center gap-1.5 italic text-terminal-600 ${className ?? ''}`}>
          <span className="flex-1">{placeholder}</span>
          <Pencil className="h-3 w-3 shrink-0 opacity-0 transition-opacity group-hover/field:opacity-100" />
        </div>
      )}
    </div>
  )
}

// ---------------------------------------------------------------------------
// Main panel
// ---------------------------------------------------------------------------

export function EmptyPanel() {
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const { data: program } = useProgram(selectedProgramId ?? '')
  const patchProgram = usePatchProgram(selectedProgramId ?? '')

  if (!selectedProgramId || !program) {
    return (
      <div className="flex flex-1 flex-col items-center justify-center gap-3 p-4 text-center">
        <MousePointerClick className="h-8 w-8 text-terminal-600" />
        <p className="text-xs text-terminal-500">Select a program to begin authoring</p>
      </div>
    )
  }

  const stateStyle = STATE_LABELS[program.state]

  return (
    <div className="flex flex-1 flex-col gap-4 overflow-y-auto p-4">
      {/* README card */}
      <div className="rounded-lg border border-terminal-600 bg-terminal-900/60">
        {/* Header bar */}
        <div className="flex items-center gap-2 border-b border-terminal-700 px-3 py-2">
          <span className="font-display text-[9px] uppercase tracking-widest text-terminal-500">
            Program
          </span>
          <span
            className={`ml-auto rounded border px-1.5 py-0.5 text-[10px] font-medium ${stateStyle.className}`}
          >
            {stateStyle.label}
          </span>
        </div>

        {/* Editable title */}
        <div className="border-b border-terminal-700 px-3 py-2.5">
          <EditableField
            value={program.name}
            placeholder="Untitled program"
            className="text-sm font-semibold text-terminal-50"
            onSave={(name) => patchProgram.mutate({ name })}
          />
        </div>

        {/* Editable description */}
        <div className="px-3 py-2.5">
          <EditableField
            value={program.description ?? ''}
            placeholder="Add a description — what does this program do?"
            multiline
            className="text-xs leading-relaxed text-terminal-300"
            onSave={(description) => patchProgram.mutate({ description })}
          />
        </div>

        {/* Metadata footer */}
        <div className="border-t border-terminal-700 px-3 py-1.5">
          <p className="text-[10px] text-terminal-600">
            Updated {new Date(program.updatedAt).toLocaleDateString(undefined, {
              year: 'numeric', month: 'short', day: 'numeric',
            })}
          </p>
        </div>
      </div>

      {/* Getting-started tips — only shown when graph is empty */}
      {Object.keys(program.graph.nodes).length === 0 && (
        <div className="space-y-2">
          <p className="font-display text-[10px] uppercase tracking-widest text-terminal-500">
            Getting started
          </p>
          <TipItem step="1" text="Drag a Trigger from the palette onto the canvas" />
          <TipItem step="2" text="Add an Agent and connect the trigger's event port to the instruction port" />
          <TipItem step="3" text="Click any node to configure it here" />
          <TipItem step="4" text="Click Save to persist your changes" />
        </div>
      )}
    </div>
  )
}

function TipItem({ step, text }: { step: string; text: string }) {
  return (
    <div className="flex items-start gap-2.5">
      <span className="flex h-4 w-4 shrink-0 items-center justify-center rounded-full bg-terminal-700 text-[9px] font-bold text-terminal-300">
        {step}
      </span>
      <p className="text-[11px] leading-relaxed text-terminal-400">{text}</p>
    </div>
  )
}
