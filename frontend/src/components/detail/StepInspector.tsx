import { AlertCircle, CheckCircle2, Clock, Loader2, Pause, SkipForward } from 'lucide-react'
import { useCanvasStore } from '../../store/canvasStore'
import { useUiStore } from '../../store/uiStore'
import { useRunDetail } from '../../api/hooks'
import { NODE_TYPE_META } from '../nodes/constants'
import type { Step, StepState } from '../../types'

// ---------------------------------------------------------------------------
// Step state badge
// ---------------------------------------------------------------------------

const STATE_CONFIG: Record<
  StepState,
  { label: string; icon: React.ReactNode; className: string }
> = {
  completed: {
    label: 'Completed',
    icon: <CheckCircle2 size={12} />,
    className: 'bg-eva-green-900/60 text-eva-green-400 border-eva-green-800',
  },
  running: {
    label: 'Running',
    icon: <Loader2 size={12} className="animate-spin" />,
    className: 'bg-magi-blue-900/60 text-magi-blue-400 border-magi-blue-800',
  },
  failed: {
    label: 'Failed',
    icon: <AlertCircle size={12} />,
    className: 'bg-nerv-red-900/60 text-nerv-red-400 border-nerv-red-800',
  },
  waiting: {
    label: 'Waiting',
    icon: <Pause size={12} />,
    className: 'bg-warn-amber-900/60 text-warn-amber-400 border-warn-amber-800',
  },
  pending: {
    label: 'Pending',
    icon: <Clock size={12} />,
    className: 'bg-terminal-800 text-terminal-400 border-terminal-600',
  },
  skipped: {
    label: 'Skipped',
    icon: <SkipForward size={12} />,
    className: 'bg-terminal-800 text-terminal-500 border-terminal-600',
  },
}

function StateBadge({ state }: { state: StepState }) {
  const cfg = STATE_CONFIG[state]
  return (
    <span
      className={`inline-flex items-center gap-1 rounded border px-2 py-0.5 text-[11px] font-medium ${cfg.className}`}
    >
      {cfg.icon}
      {cfg.label}
    </span>
  )
}

// ---------------------------------------------------------------------------
// Duration formatter
// ---------------------------------------------------------------------------

function formatDuration(startedAt?: string, finishedAt?: string): string | null {
  if (!startedAt || !finishedAt) return null
  const ms = new Date(finishedAt).getTime() - new Date(startedAt).getTime()
  if (ms < 1000) return `${ms}ms`
  if (ms < 60_000) return `${(ms / 1000).toFixed(1)}s`
  return `${Math.floor(ms / 60_000)}m ${Math.round((ms % 60_000) / 1000)}s`
}

// ---------------------------------------------------------------------------
// Payload renderer
// ---------------------------------------------------------------------------

function extractAgentOutputText(payload: unknown): string | null {
  if (typeof payload !== 'object' || payload === null) return null
  const p = payload as Record<string, unknown>
  // Message shape: { type, payload: { text }, meta }
  if (typeof p.payload === 'object' && p.payload !== null) {
    const inner = p.payload as Record<string, unknown>
    if (typeof inner.text === 'string') return inner.text
  }
  return null
}

function PayloadSection({
  label,
  value,
  nodeTypeKey,
  isOutput,
}: {
  label: string
  value: unknown
  nodeTypeKey: string
  isOutput: boolean
}) {
  if (value === null || value === undefined) {
    return (
      <div>
        <p className="mb-1 font-display text-[10px] uppercase tracking-widest text-terminal-400">
          {label}
        </p>
        <p className="text-[11px] italic text-terminal-500">—</p>
      </div>
    )
  }

  // For Agent output, show the LLM response text prominently
  if (isOutput && nodeTypeKey === 'agent') {
    const text = extractAgentOutputText(value)
    if (text) {
      return (
        <div>
          <p className="mb-1.5 font-display text-[10px] uppercase tracking-widest text-terminal-400">
            {label}
          </p>
          <p className="whitespace-pre-wrap break-words text-[12px] leading-relaxed text-terminal-100">
            {text}
          </p>
        </div>
      )
    }
  }

  // For Agent input, extract the instruction text
  if (!isOutput && nodeTypeKey === 'agent') {
    const text = extractAgentOutputText(value)
    if (text) {
      return (
        <div>
          <p className="mb-1.5 font-display text-[10px] uppercase tracking-widest text-terminal-400">
            {label}
          </p>
          <p className="whitespace-pre-wrap break-words text-[11px] leading-relaxed text-terminal-200">
            {text}
          </p>
        </div>
      )
    }
  }

  return (
    <div>
      <p className="mb-1.5 font-display text-[10px] uppercase tracking-widest text-terminal-400">
        {label}
      </p>
      <pre className="overflow-x-auto whitespace-pre-wrap break-words rounded border border-terminal-500 bg-terminal-900 p-2 font-mono text-[10px] leading-relaxed text-terminal-200">
        {JSON.stringify(value, null, 2)}
      </pre>
    </div>
  )
}

// ---------------------------------------------------------------------------
// NoStep placeholder
// ---------------------------------------------------------------------------

function NoStepPlaceholder({ hasRun }: { hasRun: boolean }) {
  return (
    <div className="flex flex-1 items-center justify-center p-6">
      <p className="text-center text-[11px] italic text-terminal-500">
        {hasRun
          ? 'This node did not run in the selected execution.'
          : 'No run selected. Switch to Operate mode to inspect execution.'}
      </p>
    </div>
  )
}

// ---------------------------------------------------------------------------
// StepInspector
// ---------------------------------------------------------------------------

export function StepInspector() {
  const selectedNodeId = useCanvasStore((s) => s.selectedNodeId)
  const nodes = useCanvasStore((s) => s.nodes)
  const inspectedRunId = useUiStore((s) => s.inspectedRunId)

  const { data: runDetail, isLoading } = useRunDetail(inspectedRunId)

  const node = nodes.find((n) => n.id === selectedNodeId)
  if (!node) return null

  const typeKey = node.data.nodeType.type
  const meta = NODE_TYPE_META[typeKey]
  const Icon = meta?.icon

  const step: Step | undefined = runDetail?.steps.find((s) => s.nodeId === selectedNodeId)
  const duration = formatDuration(step?.startedAt, step?.finishedAt)

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Header: node identity */}
      <div className="flex shrink-0 items-center gap-2 border-b border-terminal-500 px-3 py-2">
        {Icon && meta && (
          <div
            className={`flex h-5 w-5 shrink-0 items-center justify-center rounded ${meta.accentClass}`}
          >
            <Icon size={11} className="text-white" />
          </div>
        )}
        <span className="min-w-0 flex-1 truncate text-sm font-semibold text-terminal-50">
          {node.data.label}
        </span>
        <span className="shrink-0 capitalize text-[10px] text-terminal-400">{typeKey}</span>
      </div>

      {/* Step detail */}
      <div className="flex-1 overflow-y-auto">
        {isLoading ? (
          <div className="flex items-center justify-center p-6">
            <Loader2 size={16} className="animate-spin text-terminal-500" />
          </div>
        ) : !step ? (
          <NoStepPlaceholder hasRun={Boolean(inspectedRunId)} />
        ) : (
          <div className="space-y-4 p-3">
            {/* State + duration row */}
            <div className="flex items-center gap-3">
              <StateBadge state={step.state} />
              {duration && (
                <span className="flex items-center gap-1 text-[11px] text-terminal-400">
                  <Clock size={11} />
                  {duration}
                </span>
              )}
              {step.retryCount > 0 && (
                <span className="text-[11px] text-warn-amber-400">
                  {step.retryCount} retr{step.retryCount === 1 ? 'y' : 'ies'}
                </span>
              )}
            </div>

            {/* Error */}
            {step.error && (
              <div className="rounded border border-nerv-red-800 bg-nerv-red-950/60 p-2">
                <p className="mb-1 flex items-center gap-1.5 text-[10px] font-semibold uppercase tracking-wider text-nerv-red-400">
                  <AlertCircle size={11} />
                  Error
                </p>
                <p className="whitespace-pre-wrap break-words font-mono text-[10px] leading-relaxed text-nerv-red-300">
                  {step.error}
                </p>
              </div>
            )}

            {/* Input */}
            <PayloadSection
              label="Input"
              value={step.input}
              nodeTypeKey={typeKey}
              isOutput={false}
            />

            {/* Output */}
            <PayloadSection
              label="Output"
              value={step.output}
              nodeTypeKey={typeKey}
              isOutput={true}
            />
          </div>
        )}
      </div>
    </div>
  )
}
