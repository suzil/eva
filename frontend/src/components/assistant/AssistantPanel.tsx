import { useUiStore } from '../../store/uiStore'
import { useCanvasStore } from '../../store/canvasStore'
import { MessageList } from './MessageList'
import { NodeReferenceChip } from './NodeReferenceChip'

export function AssistantPanel() {
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const assistantConversations = useUiStore((s) => s.assistantConversations)

  const selectedNodeId = useCanvasStore((s) => s.selectedNodeId)
  const nodes = useCanvasStore((s) => s.nodes)

  const thread = selectedProgramId ? assistantConversations[selectedProgramId] : undefined
  const messages = thread?.messages ?? []
  const isStreaming = thread?.isStreaming ?? false

  const selectedNode = selectedNodeId ? nodes.find((n) => n.id === selectedNodeId) : undefined

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Context chip — shows selected node when one is active */}
      {selectedNode && (
        <div className="flex shrink-0 items-center gap-2 border-b border-terminal-600 px-3 py-2">
          <span className="text-xs text-terminal-400">Context:</span>
          <NodeReferenceChip
            nodeId={selectedNode.id}
            label={selectedNode.data.label}
            nodeType={selectedNode.data.nodeType.type}
            context
          />
        </div>
      )}

      {/* Message list or empty state */}
      {messages.length === 0 && !isStreaming ? (
        <div className="flex flex-1 flex-col items-center justify-center gap-2 px-6 text-center">
          <span className="font-display text-xs uppercase tracking-widest text-terminal-500">MAGI</span>
          <p className="text-xs text-terminal-400">
            Open a program and press <kbd className="rounded bg-terminal-700 px-1 py-0.5 font-mono text-terminal-200">⌘K</kbd> to talk to MAGI
          </p>
        </div>
      ) : (
        <MessageList messages={messages} isStreaming={isStreaming} />
      )}

      {/* AssistantInput — wired in EVA-89 */}
      <div className="shrink-0 border-t border-terminal-600 p-2">
        <div className="flex min-h-[40px] items-center rounded border border-terminal-600 bg-terminal-900 px-3">
          <span className="text-xs italic text-terminal-500">Input — EVA-89</span>
        </div>
      </div>
    </div>
  )
}
