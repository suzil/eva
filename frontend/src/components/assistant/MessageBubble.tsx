import { CheckCircle, XCircle, GitBranch, GitMerge, Activity, AlertCircle } from 'lucide-react'
import type { AssistantMessage } from '../../types'
import { useCanvasStore } from '../../store/canvasStore'
import { NodeReferenceChip } from './NodeReferenceChip'

interface MessageBubbleProps {
  message: AssistantMessage
}

export function MessageBubble({ message }: MessageBubbleProps) {
  switch (message.type) {
    case 'user':
      return <UserBubble text={message.text} />
    case 'text':
      return <TextBubble text={message.text} />
    case 'node_reference':
      return <NodeReferenceBubble nodeId={message.nodeId} label={message.label} />
    case 'action_result':
      return <ActionResultBubble success={message.success} message={message.message} />
    case 'graph_proposal':
      return <StubCard icon={<GitBranch className="h-4 w-4" />} label="Graph Proposal" detail={message.summary} />
    case 'graph_diff':
      return <StubCard icon={<GitMerge className="h-4 w-4" />} label="Graph Diff" detail={message.summary} />
    case 'run_data':
      return <StubCard icon={<Activity className="h-4 w-4" />} label="Run Data" detail={message.summary} />
    case 'action_confirm':
      return <StubCard icon={<AlertCircle className="h-4 w-4" />} label={message.operation} detail={message.description} />
    default:
      return null
  }
}

// ---------------------------------------------------------------------------
// Fully-rendered variants
// ---------------------------------------------------------------------------

function UserBubble({ text }: { text: string }) {
  return (
    <div className="flex justify-end px-3">
      <div className="max-w-[85%] rounded-lg bg-terminal-700 px-3 py-2 text-sm text-terminal-50 whitespace-pre-wrap">
        {text}
      </div>
    </div>
  )
}

function TextBubble({ text }: { text: string }) {
  return (
    <div className="flex justify-start px-3">
      <div className="max-w-[85%] rounded-lg bg-terminal-900 px-3 py-2 text-sm text-terminal-100 whitespace-pre-wrap">
        {text}
      </div>
    </div>
  )
}

function NodeReferenceBubble({ nodeId, label }: { nodeId: string; label: string }) {
  const node = useCanvasStore((s) => s.nodes.find((n) => n.id === nodeId))
  const nodeType = node?.data.nodeType.type

  return (
    <div className="flex justify-start px-3">
      <NodeReferenceChip nodeId={nodeId} label={label} nodeType={nodeType} />
    </div>
  )
}

function ActionResultBubble({ success, message }: { success: boolean; message: string }) {
  return (
    <div className="flex justify-start px-3">
      <div
        className={[
          'flex max-w-[85%] items-start gap-2 rounded-lg px-3 py-2 text-sm',
          success
            ? 'bg-eva-green-500/10 text-eva-green-500'
            : 'bg-nerv-red-500/10 text-nerv-red-500',
        ].join(' ')}
      >
        {success ? (
          <CheckCircle className="mt-0.5 h-4 w-4 flex-shrink-0" />
        ) : (
          <XCircle className="mt-0.5 h-4 w-4 flex-shrink-0" />
        )}
        <span>{message}</span>
      </div>
    </div>
  )
}

// ---------------------------------------------------------------------------
// Stub card — placeholder for rich variants (EVA-92, EVA-93, EVA-94)
// ---------------------------------------------------------------------------

function StubCard({
  icon,
  label,
  detail,
}: {
  icon: React.ReactNode
  label: string
  detail: string
}) {
  return (
    <div className="mx-3 rounded border border-terminal-600 bg-terminal-900 px-3 py-2">
      <div className="mb-1 flex items-center gap-1.5 text-xs font-display uppercase tracking-widest text-terminal-400">
        {icon}
        {label}
      </div>
      <p className="text-xs text-terminal-300 whitespace-pre-wrap">{detail}</p>
    </div>
  )
}
