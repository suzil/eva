import { useEffect, useRef, useState } from 'react'
import { Command, ChevronRight } from 'lucide-react'
import { useUiStore } from '../../store/uiStore'
import {
  usePrograms,
  useProgram,
  useDeployProgram,
  usePauseProgram,
  useResumeProgram,
  useCreateRun,
} from '../../api/hooks'

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

interface CommandItem {
  kind: 'command'
  id: string
  label: string
  description: string
  action: () => void
  enabled: boolean
}

interface ProgramItem {
  kind: 'program'
  id: string
  label: string
  description: string
}

interface QuestionItem {
  kind: 'question'
  id: string
  label: string
}

type Item = CommandItem | ProgramItem | QuestionItem

// ---------------------------------------------------------------------------
// Quick questions — static list
// ---------------------------------------------------------------------------

const QUICK_QUESTIONS = [
  'Explain this program',
  'Why did the last run fail?',
  'How can I improve this program?',
  'What nodes should I add?',
  'Summarize the last run',
]

// ---------------------------------------------------------------------------
// CommandBar
// ---------------------------------------------------------------------------

export function CommandBar() {
  const commandBarOpen = useUiStore((s) => s.commandBarOpen)
  const toggleCommandBar = useUiStore((s) => s.toggleCommandBar)
  const setCommandBarOpen = useUiStore((s) => s.setCommandBarOpen)
  const setDetailPanelTab = useUiStore((s) => s.setDetailPanelTab)
  const setPendingAssistantMessage = useUiStore((s) => s.setPendingAssistantMessage)
  const setSelectedProgramId = useUiStore((s) => s.setSelectedProgramId)
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)

  const [query, setQuery] = useState('')
  const [selectedIndex, setSelectedIndex] = useState(0)
  const inputRef = useRef<HTMLInputElement>(null)

  const { data: programs = [] } = usePrograms()
  const { data: currentProgram } = useProgram(selectedProgramId ?? '')

  const deployMutation = useDeployProgram(selectedProgramId ?? '')
  const pauseMutation = usePauseProgram(selectedProgramId ?? '')
  const resumeMutation = useResumeProgram(selectedProgramId ?? '')
  const createRunMutation = useCreateRun(selectedProgramId ?? '')

  // Global Cmd+K shortcut
  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if ((e.metaKey || e.ctrlKey) && e.key === 'k') {
        e.preventDefault()
        toggleCommandBar()
      }
    }
    window.addEventListener('keydown', handler)
    return () => window.removeEventListener('keydown', handler)
  }, [toggleCommandBar])

  // Reset state when opening
  useEffect(() => {
    if (commandBarOpen) {
      setQuery('')
      setSelectedIndex(0)
      setTimeout(() => inputRef.current?.focus(), 0)
    }
  }, [commandBarOpen])

  if (!commandBarOpen) return null

  // ---------------------------------------------------------------------------
  // Build item list
  // ---------------------------------------------------------------------------

  const q = query.toLowerCase().trim()

  const programState = currentProgram?.state ?? null

  const commandItems: CommandItem[] = [
    {
      kind: 'command',
      id: 'deploy',
      label: 'Deploy',
      description: 'Deploy program to active',
      enabled: !!selectedProgramId && programState === 'draft',
      action: () => deployMutation.mutate(),
    },
    {
      kind: 'command',
      id: 'run',
      label: 'Run',
      description: 'Start a new run',
      enabled: !!selectedProgramId && programState === 'active',
      action: () => createRunMutation.mutate(selectedProgramId!),
    },
    {
      kind: 'command',
      id: 'pause',
      label: 'Pause',
      description: 'Pause active program',
      enabled: !!selectedProgramId && programState === 'active',
      action: () => pauseMutation.mutate(),
    },
    {
      kind: 'command',
      id: 'resume',
      label: 'Resume',
      description: 'Resume paused program',
      enabled: !!selectedProgramId && programState === 'paused',
      action: () => resumeMutation.mutate(),
    },
    {
      kind: 'command',
      id: 'status',
      label: 'Status',
      description: currentProgram
        ? `${currentProgram.name} — ${programState ?? 'unknown'}`
        : 'No program selected',
      enabled: !!selectedProgramId,
      action: () => {
        /* display-only — closes on execute */
      },
    },
  ]

  const filteredCommands = commandItems.filter(
    (c) => !q || c.label.toLowerCase().includes(q) || c.description.toLowerCase().includes(q),
  )

  const filteredPrograms: ProgramItem[] = programs
    .filter((p) => !q || p.name.toLowerCase().includes(q))
    .map((p) => ({
      kind: 'program' as const,
      id: p.id,
      label: p.name,
      description: p.state,
    }))

  const filteredQuestions: QuestionItem[] = QUICK_QUESTIONS.filter(
    (qs) => !q || qs.toLowerCase().includes(q),
  ).map((qs) => ({ kind: 'question' as const, id: qs, label: qs }))

  const allItems: Item[] = [...filteredCommands, ...filteredPrograms, ...filteredQuestions]

  const clampedIndex = Math.min(selectedIndex, Math.max(0, allItems.length - 1))

  function close() {
    setCommandBarOpen(false)
  }

  function execute(item: Item) {
    if (item.kind === 'command') {
      if (item.enabled) item.action()
    } else if (item.kind === 'program') {
      setSelectedProgramId(item.id)
    } else {
      setDetailPanelTab('magi')
      setPendingAssistantMessage(item.label)
    }
    close()
  }

  function handleKeyDown(e: React.KeyboardEvent) {
    if (e.key === 'Escape') {
      e.preventDefault()
      close()
      return
    }
    if (e.key === 'ArrowDown') {
      e.preventDefault()
      setSelectedIndex((i) => Math.min(i + 1, allItems.length - 1))
      return
    }
    if (e.key === 'ArrowUp') {
      e.preventDefault()
      setSelectedIndex((i) => Math.max(i - 1, 0))
      return
    }
    if (e.key === 'Enter' && allItems.length > 0) {
      e.preventDefault()
      execute(allItems[clampedIndex])
    }
  }

  // ---------------------------------------------------------------------------
  // Section renderer
  // ---------------------------------------------------------------------------

  let globalIdx = 0

  function renderSection(
    label: string,
    items: Item[],
    startIdx: number,
  ): React.ReactNode {
    if (items.length === 0) return null
    return (
      <div key={label}>
        <div className="px-3 pb-1 pt-2 font-display text-[10px] uppercase tracking-widest text-terminal-500">
          {label}
        </div>
        {items.map((item, i) => {
          const idx = startIdx + i
          const isSelected = idx === clampedIndex
          const isDisabled = item.kind === 'command' && !item.enabled

          return (
            <button
              key={item.id}
              type="button"
              disabled={isDisabled}
              onMouseEnter={() => setSelectedIndex(idx)}
              onClick={() => execute(item)}
              className={[
                'flex w-full items-center gap-3 px-3 py-2 text-left transition-colors',
                isSelected && !isDisabled
                  ? 'bg-terminal-700 text-terminal-50'
                  : 'text-terminal-200',
                isDisabled ? 'cursor-not-allowed opacity-40' : 'cursor-pointer',
              ].join(' ')}
            >
              <Command className="h-3.5 w-3.5 shrink-0 text-terminal-500" />
              <span className="flex-1 truncate text-sm">{item.label}</span>
              {'description' in item && item.description && (
                <span className="truncate text-xs text-terminal-500">{item.description}</span>
              )}
              {isSelected && !isDisabled && (
                <ChevronRight className="h-3 w-3 shrink-0 text-at-field-400" />
              )}
            </button>
          )
        })}
      </div>
    )
  }

  const commandStart = 0
  globalIdx += filteredCommands.length
  const programStart = globalIdx
  globalIdx += filteredPrograms.length
  const questionStart = globalIdx

  return (
    /* Backdrop */
    <div
      className="fixed inset-0 z-50 flex items-start justify-center bg-terminal-950/80 pt-[15vh]"
      onClick={close}
    >
      {/* Modal */}
      <div
        className="w-[480px] overflow-hidden rounded border border-terminal-500 bg-terminal-800 shadow-2xl"
        onClick={(e) => e.stopPropagation()}
      >
        {/* Input row */}
        <div className="flex items-center gap-2 border-b border-terminal-600 px-3 py-2">
          <Command className="h-4 w-4 shrink-0 text-terminal-400" />
          <input
            ref={inputRef}
            type="text"
            value={query}
            onChange={(e) => {
              setQuery(e.target.value)
              setSelectedIndex(0)
            }}
            onKeyDown={handleKeyDown}
            placeholder="Search commands, programs, or ask MAGI…"
            className="flex-1 bg-transparent text-sm text-terminal-100 placeholder-terminal-500 outline-none"
          />
          <kbd className="rounded bg-terminal-700 px-1.5 py-0.5 font-mono text-[10px] text-terminal-400">
            esc
          </kbd>
        </div>

        {/* Results */}
        <div className="max-h-[360px] overflow-y-auto">
          {allItems.length === 0 ? (
            <p className="px-3 py-6 text-center text-xs text-terminal-500">No results</p>
          ) : (
            <>
              {renderSection('Commands', filteredCommands, commandStart)}
              {renderSection('Programs', filteredPrograms, programStart)}
              {renderSection('Ask MAGI', filteredQuestions, questionStart)}
            </>
          )}
        </div>

        {/* Footer hint */}
        <div className="flex items-center gap-3 border-t border-terminal-600 px-3 py-1.5">
          <span className="text-[10px] text-terminal-600">
            <kbd className="rounded bg-terminal-700 px-1 py-0.5 font-mono text-terminal-500">↑↓</kbd>
            {' '}navigate
          </span>
          <span className="text-[10px] text-terminal-600">
            <kbd className="rounded bg-terminal-700 px-1 py-0.5 font-mono text-terminal-500">↵</kbd>
            {' '}select
          </span>
          <span className="text-[10px] text-terminal-600">
            <kbd className="rounded bg-terminal-700 px-1 py-0.5 font-mono text-terminal-500">esc</kbd>
            {' '}close
          </span>
        </div>
      </div>
    </div>
  )
}
