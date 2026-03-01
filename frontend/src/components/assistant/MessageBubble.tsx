import { CheckCircle, XCircle } from 'lucide-react'
import ReactMarkdown from 'react-markdown'
import type { AssistantMessage } from '../../types'
import { useCanvasStore } from '../../store/canvasStore'
import { NodeReferenceChip } from './NodeReferenceChip'
import { GraphProposalCard } from './GraphProposalCard'
import { GraphDiffCard } from './GraphDiffCard'
import { ActionConfirmCard } from './ActionConfirmCard'
import { RunDataCard } from './RunDataCard'

interface MessageBubbleProps {
  message: AssistantMessage
  programId?: string
}

export function MessageBubble({ message, programId }: MessageBubbleProps) {
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
      return <GraphProposalCard graph={message.graph} summary={message.summary} />
    case 'graph_diff':
      return <GraphDiffCard diff={message.diff} summary={message.summary} />
    case 'run_data':
      return <RunDataCard runId={message.runId} summary={message.summary} detail={message.detail} />
    case 'action_confirm':
      return (
        <ActionConfirmCard
          operation={message.operation}
          description={message.description}
          programId={programId ?? ''}
        />
      )
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
      <div className="magi-prose max-w-[85%] rounded-lg bg-terminal-900 px-3 py-2 text-sm text-terminal-100">
        <ReactMarkdown>{text}</ReactMarkdown>
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

