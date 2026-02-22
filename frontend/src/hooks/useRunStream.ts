import { useEffect, useRef } from 'react'
import { fetchRunDetail } from '../api/client'
import { useCanvasStore } from '../store/canvasStore'
import { useUiStore } from '../store/uiStore'
import type { NodeId, RunId, WsEvent } from '../types'

interface UseRunStreamOptions {
  onLlmToken?: (nodeId: NodeId, token: string) => void
}

/**
 * Connects to the WebSocket run stream for the given runId.
 * Drives canvas step-state overlays, edge animation, and exposes LLM tokens.
 * Cleans up automatically when runId changes or the component unmounts.
 */
export function useRunStream(
  runId: RunId | null,
  _programId: string,
  opts: UseRunStreamOptions = {},
): void {
  const setNodeStepState = useCanvasStore((s) => s.setNodeStepState)
  const setNodeStepErrors = useCanvasStore((s) => s.setNodeStepErrors)
  const clearRunState = useCanvasStore((s) => s.clearRunState)
  const setActiveRunId = useUiStore((s) => s.setActiveRunId)

  // Keep opts in a ref so the effect doesn't re-run when callbacks change identity
  const optsRef = useRef(opts)
  optsRef.current = opts

  useEffect(() => {
    if (!runId) return

    // WebSocket connects to /api/ws (same origin, upgrade via dev proxy or production server)
    const wsUrl = `${window.location.protocol === 'https:' ? 'wss' : 'ws'}://${window.location.host}/api/ws`
    const ws = new WebSocket(wsUrl)
    let closed = false

    ws.onopen = () => {
      ws.send(JSON.stringify({ action: 'subscribe', topic: `run:${runId}` }))
    }

    ws.onmessage = (msg: MessageEvent<string>) => {
      let event: WsEvent
      try {
        event = JSON.parse(msg.data) as WsEvent
      } catch {
        return
      }

      switch (event.type) {
        case 'step_state':
          setNodeStepState(event.nodeId, event.state)
          break

        case 'llm_token':
          optsRef.current.onLlmToken?.(event.nodeId, event.token)
          break

        case 'log_entry':
          // EVA-28 will consume log_entry events for the Logs tab
          break

        case 'run_state': {
          const terminal = event.state === 'completed' || event.state === 'failed' || event.state === 'canceled'
          if (terminal) {
            // Fetch full run detail to get per-step error messages
            fetchRunDetail(event.runId)
              .then((detail) => {
                const errors: Record<NodeId, string> = {}
                for (const step of detail.steps) {
                  if (step.error) errors[step.nodeId] = step.error
                }
                setNodeStepErrors(errors)
              })
              .catch(() => {
                // Best-effort; errors already shown via step_state badges
              })
            setActiveRunId(null)
          }
          break
        }
      }
    }

    ws.onerror = () => {
      if (!closed) setActiveRunId(null)
    }

    ws.onclose = () => {
      closed = true
    }

    return () => {
      closed = true
      ws.close()
      // Clear overlays when we stop streaming (e.g. program switched)
      clearRunState()
    }
  }, [runId, setNodeStepState, setNodeStepErrors, clearRunState, setActiveRunId])
}
