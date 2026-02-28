import { useMemo, useState, useEffect, useRef } from 'react'
import { AlertTriangle, X, ChevronDown, ChevronUp } from 'lucide-react'
import { useUiStore } from '../../../store/uiStore'
import type { ResponseFormat } from '../../../types'

interface Props {
  nodeId: string
  systemPrompt: string
  responseFormat: ResponseFormat
  knowledgeLabels: string[]
  connectorLabels: string[]
}

const ANTI_PATTERNS = ['be helpful', 'do your best', 'be creative']
const TRANSFORMATION_PHRASES = [
  'summarize',
  'extract',
  'classify',
  'convert',
  'transform',
  'analyze',
  'analyse',
  'generate',
  'write',
  'create',
]

function computeHints(
  prompt: string,
  responseFormat: ResponseFormat,
  knowledgeLabels: string[],
  connectorLabels: string[],
): string[] {
  const hints: string[] = []
  const lower = prompt.toLowerCase()

  if (prompt.trim().length === 0) return hints

  // 1. Short prompt
  if (prompt.length < 50) {
    hints.push(
      'This prompt is very brief. Consider adding specific instructions, output format, and constraints.',
    )
  }

  // 2. Long prompt
  if (prompt.length > 3000) {
    hints.push(
      'This prompt is quite long. Consider splitting into focused sections or moving reference material into a Knowledge node.',
    )
  }

  // 3. Missing output schema
  if (
    responseFormat === 'json' &&
    !lower.includes('json') &&
    !lower.includes('format') &&
    !lower.includes('schema')
  ) {
    hints.push(
      "Response format is set to JSON but the prompt doesn't specify the expected schema or structure.",
    )
  }

  // 4. Unlinked knowledge
  for (const label of knowledgeLabels) {
    if (!lower.includes(label.toLowerCase())) {
      hints.push(
        `Knowledge node '${label}' is connected but not mentioned in the prompt.`,
      )
    }
  }

  // 5. Unlinked tools
  for (const label of connectorLabels) {
    if (!lower.includes(label.toLowerCase())) {
      hints.push(
        `Connector '${label}' is connected but not mentioned in the prompt.`,
      )
    }
  }

  // 6. Anti-patterns
  for (const phrase of ANTI_PATTERNS) {
    if (lower.includes(phrase)) {
      hints.push(
        `Vague instruction '${phrase}' rarely improves output. Consider a specific behavioral directive instead.`,
      )
    }
  }

  // 7. No examples
  const hasTransformation = TRANSFORMATION_PHRASES.some((p) => lower.includes(p))
  const hasExamples =
    lower.includes('example:') ||
    lower.includes('input:') ||
    lower.includes('for instance')
  if (prompt.length > 200 && hasTransformation && !hasExamples) {
    hints.push(
      'Complex transformation prompts benefit from 1–2 concrete input/output examples.',
    )
  }

  return hints
}

export function PromptHints({
  nodeId,
  systemPrompt,
  responseFormat,
  knowledgeLabels,
  connectorLabels,
}: Props) {
  const setDetailPanelTab = useUiStore((s) => s.setDetailPanelTab)
  const setPrefillAssistantMessage = useUiStore((s) => s.setPrefillAssistantMessage)

  const [dismissed, setDismissed] = useState(false)
  const [expanded, setExpanded] = useState(false)

  // 1s debounced prompt for hint computation — avoids flickering on every keystroke
  const [debouncedPrompt, setDebouncedPrompt] = useState(systemPrompt)
  const timerRef = useRef<ReturnType<typeof setTimeout> | null>(null)

  useEffect(() => {
    if (timerRef.current) clearTimeout(timerRef.current)
    timerRef.current = setTimeout(() => setDebouncedPrompt(systemPrompt), 1000)
    return () => {
      if (timerRef.current) clearTimeout(timerRef.current)
    }
  }, [systemPrompt])

  // Reset dismissed/expanded state when a different agent node is selected
  useEffect(() => {
    setDismissed(false)
    setExpanded(false)
  }, [nodeId])

  const hints = useMemo(
    () => computeHints(debouncedPrompt, responseFormat, knowledgeLabels, connectorLabels),
    [debouncedPrompt, responseFormat, knowledgeLabels, connectorLabels],
  )

  if (hints.length === 0 || dismissed) return null

  const handleMagiCta = () => {
    setDetailPanelTab('magi')
    setPrefillAssistantMessage('/improve')
  }

  return (
    <div className="rounded border border-warn-amber-700 bg-warn-amber-950/40">
      {/* Collapsed header row */}
      <div className="flex items-center gap-1.5 px-2.5 py-1.5">
        <AlertTriangle size={10} className="shrink-0 text-warn-amber-500" />
        <span className="flex-1 text-[10px] text-warn-amber-400">
          {hints.length} prompt suggestion{hints.length > 1 ? 's' : ''}
        </span>
        <button
          type="button"
          onClick={() => setExpanded((v) => !v)}
          className="flex items-center gap-0.5 rounded px-1.5 py-0.5 text-[10px] text-warn-amber-400 transition-colors hover:bg-warn-amber-900/40 hover:text-warn-amber-200"
        >
          {expanded ? (
            <>
              Hide <ChevronUp size={10} />
            </>
          ) : (
            <>
              View <ChevronDown size={10} />
            </>
          )}
        </button>
        <button
          type="button"
          onClick={() => setDismissed(true)}
          aria-label="Dismiss hints"
          className="rounded p-0.5 text-warn-amber-500 transition-colors hover:bg-warn-amber-900/40 hover:text-warn-amber-200"
        >
          <X size={10} />
        </button>
      </div>

      {/* Expanded list */}
      {expanded && (
        <div className="border-t border-warn-amber-800/60 px-2.5 pb-2 pt-1.5">
          <ol className="space-y-1.5">
            {hints.map((hint, i) => (
              <li key={i} className="flex gap-1.5 text-[10px] text-warn-amber-300">
                <span className="shrink-0 font-mono text-warn-amber-500">{i + 1}.</span>
                <span>{hint}</span>
              </li>
            ))}
          </ol>
          <button
            type="button"
            onClick={handleMagiCta}
            className="mt-2.5 text-[10px] text-at-field-400 underline-offset-2 transition-colors hover:text-at-field-300 hover:underline"
          >
            Get detailed suggestions from MAGI →
          </button>
        </div>
      )}
    </div>
  )
}

