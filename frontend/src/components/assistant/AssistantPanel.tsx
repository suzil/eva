import { useEffect } from 'react'
import { Trash2 } from 'lucide-react'
import { usePrograms } from '../../api/hooks'
import { useCanvasStore } from '../../store/canvasStore'
import { useUiStore } from '../../store/uiStore'
import { useAssistantStream } from '../../hooks/useAssistantStream'
import { AssistantInput } from './AssistantInput'
import { MessageList } from './MessageList'
import { NodeReferenceChip } from './NodeReferenceChip'

export function AssistantPanel() {
  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const assistantConversations = useUiStore((s) => s.assistantConversations)
  const clearAssistantConversation = useUiStore((s) => s.clearAssistantConversation)
  const pendingAssistantMessage = useUiStore((s) => s.pendingAssistantMessage)
  const setPendingAssistantMessage = useUiStore((s) => s.setPendingAssistantMessage)
  const prefillAssistantMessage = useUiStore((s) => s.prefillAssistantMessage)
  const setPrefillAssistantMessage = useUiStore((s) => s.setPrefillAssistantMessage)

  const selectedNodeId = useCanvasStore((s) => s.selectedNodeId)
  const nodes = useCanvasStore((s) => s.nodes)

  const { data: programs = [] } = usePrograms()

  const thread = selectedProgramId ? assistantConversations[selectedProgramId] : undefined
  const messages = thread?.messages ?? []
  const isStreaming = thread?.isStreaming ?? false

  const selectedNode = selectedNodeId ? nodes.find((n) => n.id === selectedNodeId) : undefined

  const { sendMessage, streamingText } = useAssistantStream(selectedProgramId, programs)

  // Auto-send a message promoted from the CommandBar (e.g. quick questions)
  useEffect(() => {
    if (pendingAssistantMessage && selectedProgramId) {
      sendMessage(pendingAssistantMessage)
      setPendingAssistantMessage(null)
    }
  }, [pendingAssistantMessage]) // eslint-disable-line react-hooks/exhaustive-deps

  function handleClear() {
    if (selectedProgramId) clearAssistantConversation(selectedProgramId)
  }

  return (
    <section aria-label="MAGI Assistant" className="flex flex-1 flex-col overflow-hidden">
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

      {/* Clear button — only when there are messages */}
      {messages.length > 0 && (
        <div className="flex shrink-0 justify-end border-b border-terminal-600 px-3 py-1">
          <button
            type="button"
            onClick={handleClear}
            aria-label="Clear conversation"
            title="Clear conversation"
            className="flex items-center gap-1 text-xs text-terminal-500 hover:text-nerv-red-400"
          >
            <Trash2 className="h-3 w-3" aria-hidden="true" />
            Clear
          </button>
        </div>
      )}

      {/* Message list or empty state */}
      {messages.length === 0 && !isStreaming ? (
        <div className="flex flex-1 flex-col items-center justify-center gap-2 px-6 text-center">
          <span className="font-display text-xs uppercase tracking-widest text-terminal-500">MAGI</span>
          <p className="text-xs text-terminal-400">
            Open a program and press <kbd className="rounded bg-terminal-700 px-1 py-0.5 font-mono text-terminal-200">⌘K</kbd> to talk to MAGI
          </p>
          <p className="text-xs text-terminal-600">
            Type <kbd className="rounded bg-terminal-700 px-1 py-0.5 font-mono text-terminal-300">/</kbd> in the input for slash commands
          </p>
        </div>
      ) : (
        <div aria-live="polite" aria-atomic="false" className="flex flex-1 flex-col overflow-hidden">
          <MessageList messages={messages} isStreaming={isStreaming} streamingText={streamingText} programId={selectedProgramId ?? undefined} />
        </div>
      )}

      {/* Input */}
      <div className="shrink-0 border-t border-terminal-600 p-2">
        <AssistantInput
          onSend={sendMessage}
          disabled={isStreaming || !selectedProgramId}
          isStreaming={isStreaming}
          initialValue={prefillAssistantMessage}
          onInitialValueConsumed={() => setPrefillAssistantMessage(null)}
        />
      </div>
    </section>
  )
}
