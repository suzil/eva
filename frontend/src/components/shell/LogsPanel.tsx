import { useEffect, useRef, useState } from 'react'
import { useUiStore } from '../../store/uiStore'
import type { LogLevel } from '../../types'

const LEVELS: LogLevel[] = ['debug', 'info', 'warn', 'error']

const LEVEL_STYLES: Record<LogLevel, string> = {
  debug: 'text-gray-500',
  info: 'text-gray-300',
  warn: 'text-amber-400',
  error: 'text-red-400',
}

const LEVEL_BADGE_STYLES: Record<LogLevel, string> = {
  debug: 'bg-gray-800 text-gray-500',
  info: 'bg-gray-800 text-gray-400',
  warn: 'bg-amber-950 text-amber-400',
  error: 'bg-red-950 text-red-400',
}

export function LogsPanel() {
  const logEntries = useUiStore((s) => s.logEntries)
  const clearRunOutput = useUiStore((s) => s.clearRunOutput)
  const bottomRef = useRef<HTMLDivElement>(null)

  // null = show all; otherwise only the selected level
  const [filter, setFilter] = useState<LogLevel | null>(null)

  const visible = filter ? logEntries.filter((e) => e.level === filter) : logEntries

  // Scroll to bottom as new entries arrive
  useEffect(() => {
    bottomRef.current?.scrollIntoView({ behavior: 'instant' })
  }, [logEntries.length])

  return (
    <div className="flex h-full flex-col">
      {/* Toolbar */}
      <div className="flex flex-shrink-0 items-center gap-1 border-b border-gray-800 px-2 py-1">
        {/* Level filter pills */}
        <button
          onClick={() => setFilter(null)}
          className={[
            'rounded px-2 py-0.5 text-xs font-medium transition-colors',
            filter === null
              ? 'bg-gray-700 text-white'
              : 'text-gray-500 hover:bg-gray-800 hover:text-gray-300',
          ].join(' ')}
        >
          All
        </button>
        {LEVELS.map((level) => (
          <button
            key={level}
            onClick={() => setFilter(filter === level ? null : level)}
            className={[
              'rounded px-2 py-0.5 text-xs font-medium capitalize transition-colors',
              filter === level
                ? LEVEL_BADGE_STYLES[level] + ' ring-1 ring-inset ring-current'
                : 'text-gray-500 hover:bg-gray-800 hover:text-gray-300',
            ].join(' ')}
          >
            {level}
          </button>
        ))}

        <div className="flex-1" />

        <button
          onClick={clearRunOutput}
          disabled={logEntries.length === 0}
          className="rounded px-2 py-0.5 text-xs text-gray-500 hover:bg-gray-800 hover:text-gray-300 disabled:cursor-not-allowed disabled:opacity-40 transition-colors"
        >
          Clear
        </button>
      </div>

      {/* Content */}
      <div className="flex-1 overflow-auto p-1">
        {visible.length === 0 ? (
          <p className="p-2 text-xs text-gray-600">
            {logEntries.length > 0 ? 'No entries match the selected filter' : 'No log entries yet'}
          </p>
        ) : (
          <table className="w-full border-collapse font-mono text-xs">
            <tbody>
              {visible.map((entry, i) => (
                <tr key={i} className="group hover:bg-gray-800/50">
                  <td className="w-[140px] select-none whitespace-nowrap py-0.5 pr-3 text-gray-600">
                    {formatTimestamp(entry.timestamp)}
                  </td>
                  <td className="w-12 py-0.5 pr-3">
                    <span
                      className={[
                        'rounded px-1 py-0.5 text-[10px] font-semibold uppercase',
                        LEVEL_BADGE_STYLES[entry.level],
                      ].join(' ')}
                    >
                      {entry.level}
                    </span>
                  </td>
                  <td className={['py-0.5 leading-relaxed', LEVEL_STYLES[entry.level]].join(' ')}>
                    {entry.message}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        )}
        <div ref={bottomRef} />
      </div>
    </div>
  )
}

function formatTimestamp(iso: string): string {
  try {
    const d = new Date(iso)
    return d.toLocaleTimeString(undefined, { hour12: false, hour: '2-digit', minute: '2-digit', second: '2-digit' })
  } catch {
    return iso
  }
}
