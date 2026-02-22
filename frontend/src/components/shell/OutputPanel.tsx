import { useEffect, useRef } from 'react'
import ReactMarkdown from 'react-markdown'
import { fetchRunDetail } from '../../api/client'
import { useUiStore } from '../../store/uiStore'

export function OutputPanel() {
  const llmOutput = useUiStore((s) => s.llmOutput)
  const activeRunId = useUiStore((s) => s.activeRunId)
  const inspectedRunId = useUiStore((s) => s.inspectedRunId)
  const runError = useUiStore((s) => s.runError)
  const clearRunOutput = useUiStore((s) => s.clearRunOutput)
  const setLlmOutput = useUiStore((s) => s.setLlmOutput)
  const bottomRef = useRef<HTMLDivElement>(null)

  // When a past run is selected (no active stream), load its agent output
  useEffect(() => {
    if (!inspectedRunId || activeRunId) return
    // Only fetch if we don't already have output for this run
    if (llmOutput) return
    fetchRunDetail(inspectedRunId).then((detail) => {
      const agentStep = detail.steps.find((s) => {
        const out = s.output as { type?: string } | null | undefined
        return out?.type === 'agent_output'
      })
      const payload = agentStep
        ? (agentStep.output as { type: string; payload?: unknown }).payload
        : undefined
      if (typeof payload === 'string' && payload.length > 0) {
        setLlmOutput(payload)
      }
    }).catch(() => {/* ignore fetch errors for past run output */})
  }, [inspectedRunId, activeRunId, llmOutput, setLlmOutput])

  // Scroll to bottom as tokens stream in
  useEffect(() => {
    bottomRef.current?.scrollIntoView({ behavior: 'instant' })
  }, [llmOutput])

  return (
    <div className="flex h-full w-full flex-col">
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
          <div className="prose prose-invert prose-sm max-w-none text-gray-200
            [&_h1]:text-sm [&_h1]:font-bold [&_h1]:text-gray-100 [&_h1]:mt-3 [&_h1]:mb-1
            [&_h2]:text-xs [&_h2]:font-bold [&_h2]:text-gray-100 [&_h2]:mt-3 [&_h2]:mb-1
            [&_h3]:text-xs [&_h3]:font-semibold [&_h3]:text-gray-200 [&_h3]:mt-2 [&_h3]:mb-0.5
            [&_p]:text-xs [&_p]:leading-relaxed [&_p]:my-1
            [&_ul]:text-xs [&_ul]:my-1 [&_ul]:pl-4
            [&_ol]:text-xs [&_ol]:my-1 [&_ol]:pl-4
            [&_li]:my-0.5
            [&_strong]:text-gray-100 [&_strong]:font-semibold
            [&_code]:text-xs [&_code]:bg-gray-800 [&_code]:px-1 [&_code]:rounded [&_code]:text-green-300
            [&_pre]:bg-gray-800 [&_pre]:p-2 [&_pre]:rounded [&_pre]:overflow-x-auto [&_pre]:text-xs
            [&_blockquote]:border-l-2 [&_blockquote]:border-gray-600 [&_blockquote]:pl-2 [&_blockquote]:text-gray-400
            [&_hr]:border-gray-700">
            <ReactMarkdown>{llmOutput}</ReactMarkdown>
            {activeRunId && (
              <span className="ml-0.5 inline-block h-3 w-1.5 animate-pulse bg-blue-400" aria-hidden />
            )}
          </div>
        ) : runError ? (
          <div className="rounded border border-red-900/60 bg-red-950/40 p-3">
            <p className="mb-1 text-xs font-semibold text-red-400">Run failed</p>
            <pre className="whitespace-pre-wrap break-words font-mono text-xs leading-relaxed text-red-300">
              {runError}
            </pre>
          </div>
        ) : (
          <p className="text-xs text-gray-600">
            {activeRunId
              ? 'Waiting for output…'
              : inspectedRunId
                ? 'Run completed with no text output — check the Logs tab for step details'
                : 'No output yet — click Run to execute the program'}
          </p>
        )}
        <div ref={bottomRef} />
      </div>
    </div>
  )
}
