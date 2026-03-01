import { useCallback, useEffect, useRef, useState } from 'react'
import { useCanvasStore } from '../store/canvasStore'
import { useUiStore } from '../store/uiStore'
import type { AssistantMessage, Program, WsEvent } from '../types'

interface AssistantContext {
  programId?: string
  programName?: string
  programState?: string
  graphSummary?: { nodeCount: number; edgeCount: number; nodeTypes: string[] }
  selectedNode?: { id: string; label: string; type: string }
  currentMode: 'author' | 'operate'
  activeRunId?: string
  recentErrors: string[]
  programList: { id: string; name: string; state: string }[]
}

interface UseAssistantStreamResult {
  sendMessage: (text: string) => void
  streamingText: string
}

/**
 * Opens a WebSocket subscription to `assistant:<programId>` and manages
 * bidirectional MAGI communication. Returns a `sendMessage` function that
 * appends a user message to the store and dispatches it over the socket,
 * plus `streamingText` for rendering the in-flight response token-by-token.
 */
export function useAssistantStream(
  programId: string | null,
  programs: Program[],
): UseAssistantStreamResult {
  const [streamingText, setStreamingText] = useState('')
  // Accumulate tokens without causing re-renders on every token
  const streamBufRef = useRef('')
  // Whether a RAF flush is currently pending (separate from the ID so that a
  // synchronous RAF mock in tests doesn't corrupt the pending state via the
  // assignment-after-callback ordering of the `id = requestAnimationFrame(cb)` pattern).
  const rafScheduledRef = useRef(false)
  // RAF ID kept only for cancelAnimationFrame on unmount.
  const rafIdRef = useRef<number | null>(null)
  // Stable ref to the live WS connection so sendMessage can access it
  const wsRef = useRef<WebSocket | null>(null)

  const mode = useUiStore((s) => s.mode)
  const activeRunId = useUiStore((s) => s.activeRunId)
  const logEntries = useUiStore((s) => s.logEntries)
  const appendAssistantMessage = useUiStore((s) => s.appendAssistantMessage)
  const setAssistantStreaming = useUiStore((s) => s.setAssistantStreaming)

  const selectedNodeId = useCanvasStore((s) => s.selectedNodeId)
  const nodes = useCanvasStore((s) => s.nodes)
  const edges = useCanvasStore((s) => s.edges)

  useEffect(() => {
    if (!programId) return

    const wsUrl = `${window.location.protocol === 'https:' ? 'wss' : 'ws'}://${window.location.host}/api/ws`
    const ws = new WebSocket(wsUrl)
    wsRef.current = ws
    let closed = false

    ws.onopen = () => {
      ws.send(JSON.stringify({ action: 'subscribe', topic: `assistant:${programId}` }))
    }

    ws.onmessage = (msg: MessageEvent<string>) => {
      let event: WsEvent
      try {
        event = JSON.parse(msg.data) as WsEvent
      } catch {
        return
      }

      if (event.type === 'assistant_token') {
        streamBufRef.current += event.token
        // Batch React state updates to at most one re-render per animation
        // frame (~16ms) instead of one per token.
        if (!rafScheduledRef.current) {
          rafScheduledRef.current = true
          rafIdRef.current = requestAnimationFrame(() => {
            setStreamingText(streamBufRef.current)
            rafScheduledRef.current = false
            rafIdRef.current = null
          })
        }
        return
      }

      if (event.type === 'assistant_reply') {
        const finalMessage: AssistantMessage = event.message
        appendAssistantMessage(programId, finalMessage)
        setAssistantStreaming(programId, false)
        streamBufRef.current = ''
        setStreamingText('')
        return
      }
    }

    ws.onerror = () => {
      if (!closed) {
        setAssistantStreaming(programId, false)
        streamBufRef.current = ''
        setStreamingText('')
      }
    }

    ws.onclose = () => {
      closed = true
      wsRef.current = null
    }

    return () => {
      closed = true
      ws.close()
      wsRef.current = null
      if (rafScheduledRef.current && rafIdRef.current !== null) {
        cancelAnimationFrame(rafIdRef.current)
        rafIdRef.current = null
        rafScheduledRef.current = false
      }
      streamBufRef.current = ''
      setStreamingText('')
    }
  }, [programId, appendAssistantMessage, setAssistantStreaming])

  const sendMessage = useCallback(
    (text: string) => {
      const ws = wsRef.current
      if (!programId || !ws || ws.readyState !== WebSocket.OPEN) return

      const trimmed = text.trim()
      if (!trimmed) return

      // Append user message to store immediately
      appendAssistantMessage(programId, { type: 'user', text: trimmed, timestamp: Date.now() })
      setAssistantStreaming(programId, true)

      const selectedNode = selectedNodeId
        ? nodes.find((n) => n.id === selectedNodeId)
        : undefined

      const recentErrors = logEntries
        .filter((e) => e.level === 'error')
        .slice(-5)
        .map((e) => e.message)

      const currentProgram = programs.find((p) => p.id === programId)

      const context: AssistantContext = {
        programId,
        programName: currentProgram?.name,
        programState: currentProgram?.state,
        graphSummary: {
          nodeCount: nodes.length,
          edgeCount: edges.length,
          nodeTypes: [...new Set(nodes.map((n) => n.data.nodeType.type))],
        },
        selectedNode: selectedNode
          ? {
              id: selectedNode.id,
              label: selectedNode.data.label,
              type: selectedNode.data.nodeType.type,
            }
          : undefined,
        currentMode: mode,
        activeRunId: activeRunId ?? undefined,
        recentErrors,
        programList: programs.map((p) => ({ id: p.id, name: p.name, state: p.state })),
      }

      ws.send(JSON.stringify({ action: 'assistant_message', content: trimmed, context }))
    },
    [
      programId,
      appendAssistantMessage,
      setAssistantStreaming,
      selectedNodeId,
      nodes,
      edges,
      mode,
      activeRunId,
      logEntries,
      programs,
    ],
  )

  return { sendMessage, streamingText }
}
