import { useEffect, useRef, useState } from 'react'
import {
  Play,
  Square,
  Rocket,
  Command,
  ChevronRight,
  Save,
  Loader2,
  PauseCircle,
  PlayCircle,
  XCircle,
  LayoutGrid,
  Undo2,
  Redo2,
} from 'lucide-react'
import { type AppMode, useUiStore } from '../../store/uiStore'
import {
  useProgram,
  useRuns,
  useRunDetail,
  useSaveGraph,
  useValidateProgram,
  useCreateRun,
  useCancelRun,
  useDeployProgram,
  usePauseProgram,
  useResumeProgram,
} from '../../api/hooks'
import { useCanvasStore } from '../../store/canvasStore'
import { useRunStream } from '../../hooks/useRunStream'
import { applyDagreLayout } from '../../lib/autoLayout'
import type { ProgramState, Run, ValidationError } from '../../types'

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
// Deploy state machine
// idle       — ready to deploy
// validating — POST validate in flight
// deploying  — POST deploy in flight
// error      — validation or deploy failed, shows error panel
// success    — deployed, banner shown briefly before auto-dismiss
// ---------------------------------------------------------------------------

type DeployPhase = 'idle' | 'validating' | 'deploying' | 'error' | 'success'

// ---------------------------------------------------------------------------
// Toolbar
// ---------------------------------------------------------------------------

export function Toolbar() {
  const mode = useUiStore((s) => s.mode)
  const setMode = useUiStore((s) => s.setMode)
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const activeRunId = useUiStore((s) => s.activeRunId)
  const setActiveRunId = useUiStore((s) => s.setActiveRunId)
  const inspectedRunId = useUiStore((s) => s.inspectedRunId)
  const setInspectedRunId = useUiStore((s) => s.setInspectedRunId)

  const { data: program } = useProgram(selectedProgramId ?? '')
  const { data: runsData } = useRuns(mode === 'operate' ? selectedProgramId : null)
  const { data: inspectedRunDetail } = useRunDetail(inspectedRunId)

  const setBottomPanelOpen = useUiStore((s) => s.setBottomPanelOpen)
  const setActiveBottomTab = useUiStore((s) => s.setActiveBottomTab)
  const clearRunOutput = useUiStore((s) => s.clearRunOutput)

  const isDirty = useCanvasStore((s) => s.isDirty)
  const buildGraph = useCanvasStore((s) => s.buildGraph)
  const markClean = useCanvasStore((s) => s.markClean)
  const clearRunState = useCanvasStore((s) => s.clearRunState)
  const loadRunSteps = useCanvasStore((s) => s.loadRunSteps)
  const setSelectedNode = useCanvasStore((s) => s.setSelectedNode)
  const nodes = useCanvasStore((s) => s.nodes)
  const edges = useCanvasStore((s) => s.edges)
  const setLayoutedNodes = useCanvasStore((s) => s.setLayoutedNodes)
  const undo = useCanvasStore((s) => s.undo)
  const redo = useCanvasStore((s) => s.redo)
  const past = useCanvasStore((s) => s.past)
  const future = useCanvasStore((s) => s.future)
  const saveMutation = useSaveGraph(selectedProgramId ?? '')

  const validateMutation = useValidateProgram(selectedProgramId ?? '')
  const createRunMutation = useCreateRun(selectedProgramId ?? '')
  const cancelRunMutation = useCancelRun()
  const deployMutation = useDeployProgram(selectedProgramId ?? '')
  const pauseMutation = usePauseProgram(selectedProgramId ?? '')
  const resumeMutation = useResumeProgram(selectedProgramId ?? '')

  // Run phase
  const [runPhase, setRunPhase] = useState<RunPhase>('idle')
  const [runErrorMsg, setRunErrorMsg] = useState<string>('')
  const runErrorTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null)

  // Deploy phase
  const [deployPhase, setDeployPhase] = useState<DeployPhase>('idle')
  const [deployErrors, setDeployErrors] = useState<ValidationError[]>([])
  const deployTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null)

  // Keep activeRunId and runPhase in sync.
  // Use the functional setter to avoid capturing a stale runPhase value in
  // the closure — the setter receives the current state at call time.
  useEffect(() => {
    if (activeRunId) {
      setRunPhase('running')
    } else {
      setRunPhase((prev) => (prev === 'running' ? 'idle' : prev))
    }
  }, [activeRunId])

  // Reset deploy phase when program changes
  useEffect(() => {
    setDeployPhase('idle')
    setDeployErrors([])
  }, [selectedProgramId])

  // When switching to Operate: auto-select the most recent run if none selected
  useEffect(() => {
    if (mode === 'operate' && runsData && runsData.length > 0 && !inspectedRunId) {
      setInspectedRunId(runsData[0].id)
    }
    if (mode === 'author') {
      clearRunState()
      setInspectedRunId(null)
    }
  }, [mode]) // eslint-disable-line react-hooks/exhaustive-deps

  // Load step states onto canvas when the inspected run detail arrives or changes
  useEffect(() => {
    if (inspectedRunDetail) {
      loadRunSteps(inspectedRunDetail.steps)
    }
  }, [inspectedRunDetail]) // eslint-disable-line react-hooks/exhaustive-deps

  // Subscribe to the active run stream (no-op when activeRunId is null)
  useRunStream(activeRunId, selectedProgramId ?? '')

  // Global keyboard shortcuts
  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      const mod = e.metaKey || e.ctrlKey
      if (!mod) return

      if (e.key === 's') {
        e.preventDefault()
        if (selectedProgramId && isDirty && !saveMutation.isPending) {
          saveMutation.mutate(buildGraph(), { onSuccess: markClean })
        }
      } else if (e.key === 'z' && !e.shiftKey) {
        e.preventDefault()
        undo()
      } else if (e.key === 'y' || (e.key === 'z' && e.shiftKey)) {
        e.preventDefault()
        redo()
      }
    }
    window.addEventListener('keydown', handler)
    return () => window.removeEventListener('keydown', handler)
  }, [selectedProgramId, isDirty, saveMutation, buildGraph, markClean, undo, redo])

  const showRunError = (msg: string) => {
    setRunErrorMsg(msg)
    setRunPhase('error')
    if (runErrorTimerRef.current) clearTimeout(runErrorTimerRef.current)
    runErrorTimerRef.current = setTimeout(() => {
      setRunPhase('idle')
      setRunErrorMsg('')
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
          showRunError(msgs || 'Graph has validation errors')
          return
        }
        setRunPhase('creating')
        createRunMutation.mutate(undefined, {
          onSuccess: (run) => {
            clearRunState()
            clearRunOutput()
            setActiveRunId(run.id)
            setBottomPanelOpen(true)
            setActiveBottomTab('output')
          },
          onError: (err) => {
            showRunError((err as Error).message || 'Failed to start run')
          },
        })
      },
      onError: (err) => {
        showRunError((err as Error).message || 'Validation failed')
      },
    })
  }

  const handleCancel = () => {
    if (!activeRunId) return
    cancelRunMutation.mutate(activeRunId, {
      onSuccess: () => setActiveRunId(null),
      onError: () => setActiveRunId(null),
    })
  }

  const handleDeploy = () => {
    if (!selectedProgramId) return
    setDeployPhase('validating')
    setDeployErrors([])

    validateMutation.mutate(undefined, {
      onSuccess: (result) => {
        if (!result.valid) {
          setDeployPhase('error')
          setDeployErrors(result.errors)
          return
        }
        setDeployPhase('deploying')
        deployMutation.mutate(undefined, {
          onSuccess: () => {
            setDeployPhase('success')
            setMode('operate')
            if (deployTimerRef.current) clearTimeout(deployTimerRef.current)
            deployTimerRef.current = setTimeout(() => {
              setDeployPhase('idle')
            }, 3000)
          },
          onError: (err) => {
            setDeployPhase('error')
            setDeployErrors([{ message: (err as Error).message || 'Deploy failed' }])
          },
        })
      },
      onError: (err) => {
        setDeployPhase('error')
        setDeployErrors([{ message: (err as Error).message || 'Validation failed' }])
      },
    })
  }

  const handlePause = () => {
    pauseMutation.mutate(undefined)
  }

  const handleResume = () => {
    resumeMutation.mutate(undefined)
  }

  const isRunBusy = runPhase === 'validating' || runPhase === 'creating'
  const isDeployBusy = deployPhase === 'validating' || deployPhase === 'deploying'
  const programState = program?.state

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

        {/* Run selector — shown in Operate mode */}
        {mode === 'operate' && selectedProgramId && (
          <RunSelector
            runs={runsData ?? []}
            selectedRunId={inspectedRunId}
            onSelect={setInspectedRunId}
          />
        )}

        {/* Action buttons — adapt to program state */}
        <div className="flex items-center gap-1">
          {/* Save — always present */}
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

          {/* Run / Cancel — shown for draft and active programs */}
          {(programState === 'draft' || programState === 'active' || programState === undefined) && (
            <>
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
                  label={
                    runPhase === 'validating'
                      ? 'Validating…'
                      : runPhase === 'creating'
                        ? 'Starting…'
                        : 'Run'
                  }
                  onClick={handleRun}
                  disabled={!selectedProgramId || isRunBusy || runPhase === 'error'}
                />
              )}
            </>
          )}

          {/* Deploy — Draft only */}
          {programState === 'draft' && (
            <ToolbarButton
              icon={
                isDeployBusy ? (
                  <Loader2 className="h-3.5 w-3.5 animate-spin" />
                ) : (
                  <Rocket className="h-3.5 w-3.5" />
                )
              }
              label={
                deployPhase === 'validating'
                  ? 'Validating…'
                  : deployPhase === 'deploying'
                    ? 'Deploying…'
                    : 'Deploy'
              }
              onClick={handleDeploy}
              disabled={!selectedProgramId || isDeployBusy}
              variant="primary"
            />
          )}

          {/* Pause — Active only */}
          {programState === 'active' && (
            <ToolbarButton
              icon={
                pauseMutation.isPending ? (
                  <Loader2 className="h-3.5 w-3.5 animate-spin" />
                ) : (
                  <PauseCircle className="h-3.5 w-3.5" />
                )
              }
              label="Pause"
              onClick={handlePause}
              disabled={pauseMutation.isPending}
            />
          )}

          {/* Resume — Paused only */}
          {programState === 'paused' && (
            <ToolbarButton
              icon={
                resumeMutation.isPending ? (
                  <Loader2 className="h-3.5 w-3.5 animate-spin" />
                ) : (
                  <PlayCircle className="h-3.5 w-3.5" />
                )
              }
              label="Resume"
              onClick={handleResume}
              disabled={resumeMutation.isPending}
              variant="primary"
            />
          )}

          {/* Undo / Redo — Author mode only */}
          {mode === 'author' && (
            <>
              <ToolbarButton
                icon={<Undo2 className="h-3.5 w-3.5" />}
                label="Undo (⌘Z)"
                onClick={undo}
                disabled={past.length === 0}
                hideLabel
              />
              <ToolbarButton
                icon={<Redo2 className="h-3.5 w-3.5" />}
                label="Redo (⌘Y)"
                onClick={redo}
                disabled={future.length === 0}
                hideLabel
              />
            </>
          )}

          {/* Auto-layout — Author mode only */}
          {mode === 'author' && (
            <ToolbarButton
              icon={<LayoutGrid className="h-3.5 w-3.5" />}
              label="Auto-layout"
              onClick={() => setLayoutedNodes(applyDagreLayout(nodes, edges))}
              disabled={nodes.length === 0}
              hideLabel
            />
          )}

          {/* Command palette placeholder */}
          <ToolbarButton
            icon={<Command className="h-3.5 w-3.5" />}
            label="⌘K"
            onClick={() => {}}
            disabled
            hideLabel
          />
        </div>
      </header>

      {/* Run validation / error banner */}
      {runPhase === 'error' && runErrorMsg && (
        <div className="flex items-center gap-2 border-b border-red-900/60 bg-red-950/50 px-3 py-1.5 text-xs text-red-400">
          <span className="shrink-0 font-semibold">Run error:</span>
          <span className="truncate">{runErrorMsg}</span>
          <button
            onClick={() => {
              setRunPhase('idle')
              setRunErrorMsg('')
            }}
            className="ml-auto shrink-0 text-red-600 hover:text-red-400"
            aria-label="Dismiss"
          >
            ✕
          </button>
        </div>
      )}

      {/* Deploy success banner */}
      {deployPhase === 'success' && (
        <div className="flex items-center gap-2 border-b border-green-900/60 bg-green-950/50 px-3 py-1.5 text-xs text-green-400">
          <span className="shrink-0 font-semibold">Deployed.</span>
          <span>Program is now active — switched to Operate mode.</span>
          <button
            onClick={() => setDeployPhase('idle')}
            className="ml-auto shrink-0 text-green-700 hover:text-green-400"
            aria-label="Dismiss"
          >
            ✕
          </button>
        </div>
      )}

      {/* Deploy error panel — list of clickable validation errors */}
      {deployPhase === 'error' && deployErrors.length > 0 && (
        <div className="border-b border-red-900/60 bg-red-950/50 px-3 py-2 text-xs text-red-400">
          <div className="mb-1.5 flex items-center gap-2">
            <span className="font-semibold">Deploy failed — fix the following issues:</span>
            <button
              onClick={() => {
                setDeployPhase('idle')
                setDeployErrors([])
              }}
              className="ml-auto shrink-0 text-red-600 hover:text-red-400"
              aria-label="Dismiss"
            >
              <XCircle className="h-3.5 w-3.5" />
            </button>
          </div>
          <ul className="space-y-0.5">
            {deployErrors.map((err, i) => (
              <li key={i}>
                {err.nodeId ? (
                  <button
                    className="text-left text-red-300 underline decoration-red-700 underline-offset-2 hover:text-red-100"
                    onClick={() => setSelectedNode(err.nodeId!)}
                  >
                    {err.message}
                  </button>
                ) : (
                  <span className="text-red-300">{err.message}</span>
                )}
              </li>
            ))}
          </ul>
        </div>
      )}
    </div>
  )
}

// ---------------------------------------------------------------------------
// RunSelector — dropdown to pick which historical run to inspect
// ---------------------------------------------------------------------------

function formatRunLabel(run: Run): string {
  const date = run.startedAt ? new Date(run.startedAt) : null
  const dateStr = date
    ? date.toLocaleString(undefined, {
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit',
      })
    : 'Pending'
  const stateEmoji = {
    completed: '✓',
    failed: '✗',
    running: '●',
    canceled: '○',
    waiting: '⧗',
    pending: '○',
  }[run.state] ?? ''
  return `${stateEmoji} ${dateStr}`
}

function RunSelector({
  runs,
  selectedRunId,
  onSelect,
}: {
  runs: Run[]
  selectedRunId: string | null
  onSelect: (id: string | null) => void
}) {
  if (runs.length === 0) {
    return (
      <span className="text-[11px] text-gray-600 italic">No runs yet</span>
    )
  }
  return (
    <select
      value={selectedRunId ?? ''}
      onChange={(e) => onSelect(e.target.value || null)}
      className="rounded border border-gray-700 bg-gray-800 px-2 py-1 text-[11px] text-gray-300 focus:border-gray-500 focus:outline-none"
      aria-label="Select run to inspect"
    >
      {runs.map((run) => (
        <option key={run.id} value={run.id}>
          {formatRunLabel(run)}
        </option>
      ))}
    </select>
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
