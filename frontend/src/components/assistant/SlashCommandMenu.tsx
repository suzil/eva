import { useEffect, useRef, useState } from 'react'

interface SlashCommand {
  command: string
  description: string
}

const SLASH_COMMANDS: SlashCommand[] = [
  { command: '/generate', description: 'Start program generation' },
  { command: '/explain',  description: 'Explain current program or selected node' },
  { command: '/modify',   description: 'Propose graph modification' },
  { command: '/improve',  description: 'Suggest prompt improvements for selected agent' },
  { command: '/debug',    description: 'Analyze a run failure' },
  { command: '/find',     description: 'Search programs' },
  { command: '/run',      description: 'Run current program' },
  { command: '/deploy',   description: 'Deploy current program' },
  { command: '/pause',    description: 'Pause current program' },
  { command: '/resume',   description: 'Resume paused program' },
  { command: '/status',   description: 'Check program or run status' },
]

interface SlashCommandMenuProps {
  /** The text after the `/` used to filter commands. */
  query: string
  onSelect: (command: string) => void
  onClose: () => void
}

/**
 * Autocomplete dropdown for slash commands. Positioned above the input
 * via `bottom-full`. The parent is responsible for showing/hiding this
 * component; keyboard events are forwarded from the textarea via the
 * `onKeyDown` prop returned by this component's sibling `AssistantInput`.
 */
export function SlashCommandMenu({ query, onSelect, onClose }: SlashCommandMenuProps) {
  const filtered = SLASH_COMMANDS.filter((c) =>
    c.command.startsWith('/' + query.toLowerCase()),
  )

  const [highlightedIdx, setHighlightedIdx] = useState(0)
  const itemRefs = useRef<(HTMLButtonElement | null)[]>([])

  // Reset highlight when the filtered list changes
  useEffect(() => {
    setHighlightedIdx(0)
  }, [query])

  // Scroll highlighted item into view
  useEffect(() => {
    itemRefs.current[highlightedIdx]?.scrollIntoView({ block: 'nearest' })
  }, [highlightedIdx])

  // Keyboard navigation delegated from the parent textarea via a document listener
  useEffect(() => {
    function handleKeyDown(e: KeyboardEvent) {
      if (e.key === 'ArrowDown') {
        e.preventDefault()
        setHighlightedIdx((i) => (i + 1) % Math.max(filtered.length, 1))
      } else if (e.key === 'ArrowUp') {
        e.preventDefault()
        setHighlightedIdx((i) => (i - 1 + Math.max(filtered.length, 1)) % Math.max(filtered.length, 1))
      } else if (e.key === 'Enter') {
        e.preventDefault()
        if (filtered[highlightedIdx]) {
          onSelect(filtered[highlightedIdx].command)
        }
      } else if (e.key === 'Escape') {
        e.preventDefault()
        onClose()
      }
    }

    document.addEventListener('keydown', handleKeyDown, { capture: true })
    return () => document.removeEventListener('keydown', handleKeyDown, { capture: true })
  }, [filtered, highlightedIdx, onSelect, onClose])

  if (filtered.length === 0) return null

  return (
    <div className="absolute bottom-full left-0 right-0 mb-1 max-h-48 overflow-y-auto rounded border border-terminal-600 bg-terminal-800 shadow-lg">
      {filtered.map((cmd, idx) => (
        <button
          key={cmd.command}
          ref={(el) => { itemRefs.current[idx] = el }}
          type="button"
          className={[
            'flex w-full items-baseline gap-2 px-3 py-1.5 text-left',
            idx === highlightedIdx
              ? 'bg-magi-blue-500/20 text-terminal-50'
              : 'text-terminal-300 hover:bg-terminal-700',
          ].join(' ')}
          onMouseEnter={() => setHighlightedIdx(idx)}
          onMouseDown={(e) => {
            // Prevent textarea blur before click registers
            e.preventDefault()
            onSelect(cmd.command)
          }}
        >
          <span className="font-mono text-xs text-magi-blue-400">{cmd.command}</span>
          <span className="text-xs text-terminal-500">{cmd.description}</span>
        </button>
      ))}
    </div>
  )
}
