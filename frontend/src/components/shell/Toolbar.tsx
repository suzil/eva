import { Play, Rocket, Command, ChevronRight, Save } from 'lucide-react'
import { type AppMode, useUiStore } from '../../store/uiStore'
import { useProgram, useSaveGraph } from '../../api/hooks'
import { useCanvasStore } from '../../store/canvasStore'
import type { ProgramState } from '../../types'

// ---------------------------------------------------------------------------
// Breadcrumb badge — mirrors ProgramsList StateBadge but lighter weight
// ---------------------------------------------------------------------------

const BREADCRUMB_BADGE_STYLES: Record<ProgramState, string> = {
  draft: 'border-gray-700 text-gray-500',
  active: 'border-green-800 text-green-500',
  paused: 'border-amber-800 text-amber-500',
  archived: 'border-gray-700 text-gray-600',
}

// ---------------------------------------------------------------------------
// Toolbar
// ---------------------------------------------------------------------------

export function Toolbar() {
  const mode = useUiStore((s) => s.mode)
  const setMode = useUiStore((s) => s.setMode)
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)

  const { data: program } = useProgram(selectedProgramId ?? '')

  const isDirty = useCanvasStore((s) => s.isDirty)
  const buildGraph = useCanvasStore((s) => s.buildGraph)
  const markClean = useCanvasStore((s) => s.markClean)
  const saveMutation = useSaveGraph(selectedProgramId ?? '')

  const handleSave = () => {
    if (!selectedProgramId) return
    saveMutation.mutate(buildGraph(), { onSuccess: markClean })
  }

  return (
    <header className="flex h-11 flex-shrink-0 items-center gap-3 border-b border-gray-800 bg-gray-900 px-3">
      {/* Breadcrumb */}
      <nav className="flex items-center gap-1 text-sm text-gray-400" aria-label="Breadcrumb">
        <span className="cursor-default hover:text-white">Programs</span>
        {program ? (
          <>
            <ChevronRight className="h-3.5 w-3.5" />
            <span className="font-medium text-white">{program.name}</span>
            <span
              className={`ml-1 rounded border px-1.5 py-0.5 text-xs ${BREADCRUMB_BADGE_STYLES[program.state]}`}
            >
              {program.state}
            </span>
          </>
        ) : (
          <>
            <ChevronRight className="h-3.5 w-3.5" />
            <span className="text-gray-600 italic">Select a program</span>
          </>
        )}
      </nav>

      <div className="flex-1" />

      {/* Author / Operate toggle */}
      <ModeToggle mode={mode} onChange={setMode} />

      {/* Action buttons */}
      <div className="flex items-center gap-1">
        <ToolbarButton
          icon={
            <span className="relative flex items-center">
              <Save className="h-3.5 w-3.5" />
              {isDirty && (
                <span className="absolute -right-1.5 -top-1.5 h-1.5 w-1.5 rounded-full bg-amber-400" />
              )}
            </span>
          }
          label="Save"
          onClick={handleSave}
          disabled={!selectedProgramId || !isDirty || saveMutation.isPending}
        />
        <ToolbarButton
          icon={<Play className="h-3.5 w-3.5" />}
          label="Run"
          onClick={() => {}}
          disabled
        />
        <ToolbarButton
          icon={<Rocket className="h-3.5 w-3.5" />}
          label="Deploy"
          onClick={() => {}}
          disabled
          variant="primary"
        />
        <ToolbarButton
          icon={<Command className="h-3.5 w-3.5" />}
          label="⌘K"
          onClick={() => {}}
          disabled
          hideLabel
        />
      </div>
    </header>
  )
}

function ModeToggle({ mode, onChange }: { mode: AppMode; onChange: (m: AppMode) => void }) {
  return (
    <div
      className="flex rounded-md border border-gray-700 bg-gray-800 p-0.5 text-xs font-medium"
      role="group"
      aria-label="App mode"
    >
      {(['author', 'operate'] as AppMode[]).map((m) => (
        <button
          key={m}
          onClick={() => onChange(m)}
          className={[
            'rounded px-2.5 py-1 capitalize transition-colors',
            mode === m
              ? 'bg-gray-600 text-white'
              : 'text-gray-400 hover:text-white',
          ].join(' ')}
          aria-pressed={mode === m}
        >
          {m}
        </button>
      ))}
    </div>
  )
}

interface ToolbarButtonProps {
  icon: React.ReactNode
  label: string
  onClick: () => void
  disabled?: boolean
  variant?: 'default' | 'primary'
  hideLabel?: boolean
}

function ToolbarButton({ icon, label, onClick, disabled, variant = 'default', hideLabel }: ToolbarButtonProps) {
  const base = 'flex items-center gap-1.5 rounded px-2.5 py-1 text-xs font-medium transition-colors disabled:cursor-not-allowed disabled:opacity-40'
  const styles = {
    default: 'bg-gray-800 text-gray-300 hover:bg-gray-700 hover:text-white border border-gray-700',
    primary: 'bg-blue-600 text-white hover:bg-blue-500 border border-blue-500',
  }

  return (
    <button
      onClick={onClick}
      disabled={disabled}
      title={label}
      aria-label={label}
      className={`${base} ${styles[variant]}`}
    >
      {icon}
      {!hideLabel && <span>{label}</span>}
    </button>
  )
}
