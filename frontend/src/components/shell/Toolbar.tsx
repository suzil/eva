import { useEffect, useRef, useState } from 'react'
import { Play, Square, Rocket, Command, ChevronRight, Save, Loader2 } from 'lucide-react'
import { type AppMode, useUiStore } from '../../store/uiStore'
import { useProgram, useSaveGraph, useValidateProgram, useCreateRun, useCancelRun } from '../../api/hooks'
import { useCanvasStore } from '../../store/canvasStore'
import { useRunStream } from '../../hooks/useRunStream'
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
// Run button state machine
// idle       — ready to run
// validating — POST validate in flight
// creating   — POST runs in flight
// running    — WebSocket stream active
// error      — validation or creation failed, shows banner for 5s
// ---------------------------------------------------------------------------

type RunPhase = 'idle' | 'validating' | 'creating' | 'running' | 'error'

// ---------------------------------------------------------------------------
// Toolbar
// ---------------------------------------------------------------------------

export function Toolbar() {
  const mode = useUiStore((s) => s.mode)
  const setMode = useUiStore((s) => s.setMode)
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const activeRunId = useUiStore((s) => s.activeRunId)
  const setActiveRunId = useUiStore((s) => s.setActiveRunId)

  const { data: program } = useProgram(selectedProgramId ?? '')

  const isDirty = useCanvasStore((s) => s.isDirty)
  const buildGraph = useCanvasStore((s) => s.buildGraph)
  const markClean = useCanvasStore((s) => s.markClean)
  const clearRunState = useCanvasStore((s) => s.clearRunState)
  const saveMutation = useSaveGraph(selectedProgramId ?? '')

  const validateMutation = useValidateProgram(selectedProgramId ?? '')
  const createRunMutation = useCreateRun(selectedProgramId ?? '')
  const cancelRunMutation = useCancelRun()

  const [runPhase, setRunPhase] = useState<RunPhase>('idle')
  const [errorMsg, setErrorMsg] = useState<string>('')
  const errorTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null)

  // Keep activeRunId and runPhase in sync
  useEffect(() => {
    if (activeRunId) {
      setRunPhase('running')
    } else if (runPhase === 'running') {
      setRunPhase('idle')
    }
  }, [activeRunId]) // eslint-disable-line react-hooks/exhaustive-deps

  // Subscribe to the active run stream (no-op when activeRunId is null)
  useRunStream(activeRunId, selectedProgramId ?? '')

  const showError = (msg: string) => {
    setErrorMsg(msg)
    setRunPhase('error')
    if (errorTimerRef.current) clearTimeout(errorTimerRef.current)
    errorTimerRef.current = setTimeout(() => {
      setRunPhase('idle')
      setErrorMsg('')
    }, 5000)
  }

  const handleSave = () => {
    if (!selectedProgramId) return
    saveMutation.mutate(buildGraph(), { onSuccess: markClean })
  }

  const handleRun = () => {
    if (!selectedProgramId) return
    setRunPhase('validating')

    validateMutation.mutate(undefined, {
      onSuccess: (result) => {
        if (!result.valid) {
          const msgs = result.errors.map((e) => e.message).join('; ')
          showError(msgs || 'Graph has validation errors')
          return
        }
        setRunPhase('creating')
        createRunMutation.mutate(undefined, {
          onSuccess: (run) => {
            clearRunState()
            setActiveRunId(run.id)
            // runPhase transitions to 'running' via the activeRunId effect above
          },
          onError: (err) => {
            showError((err as Error).message || 'Failed to start run')
          },
        })
      },
      onError: (err) => {
        showError((err as Error).message || 'Validation failed')
      },
    })
  }

  const handleCancel = () => {
    if (!activeRunId) return
    cancelRunMutation.mutate(activeRunId, {
      onSuccess: () => {
        setActiveRunId(null)
      },
      onError: () => {
        // Run may have already finished; clear anyway
        setActiveRunId(null)
      },
    })
  }

  const isRunBusy = runPhase === 'validating' || runPhase === 'creating'

  return (
    <div className="flex flex-col flex-shrink-0">
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

          {runPhase === 'running' ? (
            <ToolbarButton
              icon={<Square className="h-3.5 w-3.5" />}
              label="Cancel"
              onClick={handleCancel}
              disabled={cancelRunMutation.isPending}
              variant="danger"
            />
          ) : (
            <ToolbarButton
              icon={
                isRunBusy ? (
                  <Loader2 className="h-3.5 w-3.5 animate-spin" />
                ) : (
                  <Play className="h-3.5 w-3.5" />
                )
              }
              label={runPhase === 'validating' ? 'Validating…' : runPhase === 'creating' ? 'Starting…' : 'Run'}
              onClick={handleRun}
              disabled={!selectedProgramId || isRunBusy || runPhase === 'error'}
            />
          )}

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

      {/* Validation / run error banner */}
      {runPhase === 'error' && errorMsg && (
        <div className="flex items-center gap-2 border-b border-red-900/60 bg-red-950/50 px-3 py-1.5 text-xs text-red-400">
          <span className="shrink-0 font-semibold">Run error:</span>
          <span className="truncate">{errorMsg}</span>
          <button
            onClick={() => { setRunPhase('idle'); setErrorMsg('') }}
            className="ml-auto shrink-0 text-red-600 hover:text-red-400"
            aria-label="Dismiss"
          >
            ✕
          </button>
        </div>
      )}
    </div>
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
  variant?: 'default' | 'primary' | 'danger'
  hideLabel?: boolean
}

function ToolbarButton({ icon, label, onClick, disabled, variant = 'default', hideLabel }: ToolbarButtonProps) {
  const base = 'flex items-center gap-1.5 rounded px-2.5 py-1 text-xs font-medium transition-colors disabled:cursor-not-allowed disabled:opacity-40'
  const styles = {
    default: 'bg-gray-800 text-gray-300 hover:bg-gray-700 hover:text-white border border-gray-700',
    primary: 'bg-blue-600 text-white hover:bg-blue-500 border border-blue-500',
    danger: 'bg-red-900/60 text-red-300 hover:bg-red-800/60 border border-red-800',
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
