import { useEffect, useRef } from 'react'
import { useUiStore } from '../../store/uiStore'

export function OutputPanel() {
  const llmOutput = useUiStore((s) => s.llmOutput)
  const activeRunId = useUiStore((s) => s.activeRunId)
  const clearRunOutput = useUiStore((s) => s.clearRunOutput)
  const bottomRef = useRef<HTMLDivElement>(null)

  // Scroll to bottom as tokens stream in
  useEffect(() => {
    bottomRef.current?.scrollIntoView({ behavior: 'instant' })
  }, [llmOutput])

  return (
    <div className="flex h-full flex-col">
      {/* Toolbar */}
      <div className="flex flex-shrink-0 items-center justify-end border-b border-gray-800 px-2 py-1">
        <button
          onClick={clearRunOutput}
          disabled={!llmOutput}
          className="rounded px-2 py-0.5 text-xs text-gray-500 hover:bg-gray-800 hover:text-gray-300 disabled:cursor-not-allowed disabled:opacity-40 transition-colors"
        >
          Clear
        </button>
      </div>

      {/* Content */}
      <div className="flex-1 overflow-auto p-3">
        {llmOutput ? (
          <pre className="whitespace-pre-wrap break-words font-mono text-xs leading-relaxed text-gray-200">
            {llmOutput}
            {activeRunId && (
              <span className="ml-0.5 inline-block h-3 w-1.5 animate-pulse bg-blue-400" aria-hidden />
            )}
          </pre>
        ) : (
          <p className="text-xs text-gray-600">
            {activeRunId ? 'Waiting for output…' : 'No output yet — click Run to execute the program'}
          </p>
        )}
        <div ref={bottomRef} />
      </div>
    </div>
  )
}
