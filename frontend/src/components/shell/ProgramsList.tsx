import { useRef, useState } from 'react'
import { Plus, LayoutList, Loader2 } from 'lucide-react'
import { usePrograms, useCreateProgram, usePatchProgram } from '../../api/hooks'
import { useUiStore } from '../../store/uiStore'
import { useCanvasStore } from '../../store/canvasStore'
import type { ProgramState } from '../../types'

// ---------------------------------------------------------------------------
// Badge
// ---------------------------------------------------------------------------

const BADGE_STYLES: Record<ProgramState, string> = {
  draft: 'border-terminal-500 text-terminal-400',
  active: 'border-eva-green-700 text-eva-green-400',
  paused: 'border-warn-amber-700 text-warn-amber-400',
  archived: 'border-terminal-500 text-terminal-500',
}

function StateBadge({ state }: { state: ProgramState }) {
  return (
    <span
      className={`ml-auto flex-shrink-0 rounded border px-1.5 py-0.5 text-[10px] font-medium capitalize ${BADGE_STYLES[state]}`}
    >
      {state}
    </span>
  )
}

// ---------------------------------------------------------------------------
// ProgramItem
// ---------------------------------------------------------------------------

interface ProgramItemProps {
  id: string
  name: string
  state: ProgramState
  isSelected: boolean
  isRenaming: boolean
  renameValue: string
  onSelect: () => void
  onRenameStart: () => void
  onRenameChange: (value: string) => void
  onRenameCommit: () => void
}

function ProgramItem({
  id,
  name,
  state,
  isSelected,
  isRenaming,
  renameValue,
  onSelect,
  onRenameStart,
  onRenameChange,
  onRenameCommit,
}: ProgramItemProps) {
  const patchProgram = usePatchProgram(id)
  const inputRef = useRef<HTMLInputElement>(null)

  const commitRename = () => {
    const trimmed = renameValue.trim()
    if (trimmed && trimmed !== name) {
      patchProgram.mutate({ name: trimmed })
    }
    onRenameCommit()
  }

  const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Enter') {
      e.preventDefault()
      commitRename()
    } else if (e.key === 'Escape') {
      onRenameCommit()
    }
  }

  return (
    <div
      role="button"
      tabIndex={0}
      onClick={onSelect}
      onKeyDown={(e) => e.key === 'Enter' && onSelect()}
      className={[
        'group relative flex min-h-[34px] cursor-pointer items-center gap-2 px-3 py-1.5 transition-colors',
        isSelected
          ? 'bg-terminal-600 text-terminal-50'
          : 'text-terminal-200 hover:bg-terminal-600 hover:text-terminal-100',
      ].join(' ')}
    >
      {isSelected && (
        <span className="absolute left-0 top-1/2 h-4 w-0.5 -translate-y-1/2 rounded-r bg-at-field-500" />
      )}

      {isRenaming ? (
        <input
          ref={inputRef}
          autoFocus
          value={renameValue}
          onChange={(e) => onRenameChange(e.target.value)}
          onBlur={commitRename}
          onKeyDown={handleKeyDown}
          onClick={(e) => e.stopPropagation()}
          className="min-w-0 flex-1 rounded bg-terminal-700 px-1.5 py-0.5 text-xs text-terminal-100 outline-none ring-1 ring-at-field-500"
        />
      ) : (
        <span
          className="min-w-0 flex-1 truncate text-xs"
          onDoubleClick={(e) => {
            e.stopPropagation()
            onRenameStart()
          }}
        >
          {name}
        </span>
      )}

      <StateBadge state={state} />
    </div>
  )
}

// ---------------------------------------------------------------------------
// ProgramsPanel — exported component used by SidePanel
// ---------------------------------------------------------------------------

export function ProgramsPanel() {
  const { data: programs, isLoading, isError, refetch } = usePrograms()
  const createProgram = useCreateProgram()
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const setSelectedProgramId = useUiStore((s) => s.setSelectedProgramId)
  const isDirty = useCanvasStore((s) => s.isDirty)

  const [renamingId, setRenamingId] = useState<string | null>(null)
  const [renameValue, setRenameValue] = useState('')

  const handleSelectProgram = (id: string) => {
    if (isDirty && selectedProgramId !== id) {
      if (!window.confirm('You have unsaved changes. Switch programs anyway?')) return
    }
    setSelectedProgramId(id)
  }

  const handleCreate = () => {
    createProgram.mutate(
      { name: 'Untitled' },
      {
        onSuccess: (program) => {
          setSelectedProgramId(program.id)
          setRenamingId(program.id)
          setRenameValue(program.name)
        },
      },
    )
  }

  const list = programs ?? []

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Sub-header with create button */}
      <div className="flex items-center justify-between border-b border-terminal-500 px-3 py-1.5">
        <span className="font-display text-xs uppercase tracking-widest text-terminal-300">
          Programs
        </span>
        <button
          onClick={handleCreate}
          disabled={createProgram.isPending}
          title="New program"
          aria-label="New program"
          className="flex h-5 w-5 items-center justify-center rounded text-terminal-400 transition-colors hover:bg-terminal-600 hover:text-terminal-100 disabled:opacity-40"
        >
          <Plus className="h-3.5 w-3.5" />
        </button>
      </div>

      {/* Body */}
      {isLoading ? (
        <div className="flex flex-1 items-center justify-center p-4">
          <Loader2 className="h-4 w-4 animate-spin text-terminal-400" />
        </div>
      ) : isError ? (
        <div className="flex flex-1 flex-col items-center justify-center gap-2 p-4">
          <p className="text-center text-xs text-nerv-red-400">Failed to load programs</p>
          <button
            onClick={() => refetch()}
            className="rounded border border-terminal-500 px-2.5 py-1 text-xs text-terminal-400 transition-colors hover:border-terminal-400 hover:text-terminal-100"
          >
            Try again
          </button>
        </div>
      ) : list.length === 0 ? (
        <div className="flex flex-1 flex-col items-center justify-center gap-3 p-4">
          <LayoutList className="h-8 w-8 text-terminal-600" />
          <p className="text-center text-xs text-terminal-400">No programs yet</p>
          <button
            onClick={handleCreate}
            disabled={createProgram.isPending}
            className="rounded bg-at-field-500 px-3 py-1.5 text-xs font-medium text-terminal-950 hover:bg-at-field-600 disabled:opacity-50"
          >
            Create your first program
          </button>
        </div>
      ) : (
        <div className="flex flex-1 flex-col overflow-y-auto">
          {list.map((program) => (
            <ProgramItem
              key={program.id}
              id={program.id}
              name={program.name}
              state={program.state}
              isSelected={selectedProgramId === program.id}
              isRenaming={renamingId === program.id}
              renameValue={renamingId === program.id ? renameValue : program.name}
              onSelect={() => handleSelectProgram(program.id)}
              onRenameStart={() => {
                setRenamingId(program.id)
                setRenameValue(program.name)
              }}
              onRenameChange={setRenameValue}
              onRenameCommit={() => setRenamingId(null)}
            />
          ))}
        </div>
      )}
    </div>
  )
}
