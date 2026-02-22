import { useEffect } from 'react'
import { useQueryClient } from '@tanstack/react-query'
import { fetchRunDetail } from '../api/client'
import { runKeys } from '../api/hooks'
import { useCanvasStore } from '../store/canvasStore'
import { useUiStore } from '../store/uiStore'
import type { NodeId, RunId, WsEvent } from '../types'

/**
 * Connects to the WebSocket run stream for the given runId.
 * Drives canvas step-state overlays, edge animation, LLM token streaming,
 * and log entry accumulation. Cleans up automatically when runId changes
 * or the component unmounts.
 */
export function useRunStream(runId: RunId | null, programId: string): void {
  const queryClient = useQueryClient()
  const setNodeStepState = useCanvasStore((s) => s.setNodeStepState)
  const setNodeStepErrors = useCanvasStore((s) => s.setNodeStepErrors)
  const clearRunState = useCanvasStore((s) => s.clearRunState)
  const setActiveRunId = useUiStore((s) => s.setActiveRunId)
  const appendLlmToken = useUiStore((s) => s.appendLlmToken)
  const appendLogEntry = useUiStore((s) => s.appendLogEntry)
  const clearRunOutput = useUiStore((s) => s.clearRunOutput)
  const setRunError = useUiStore((s) => s.setRunError)

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
          appendLlmToken(event.token)
          break

        case 'log_entry':
          appendLogEntry({
            stepId: event.stepId,
            level: event.level,
            message: event.message,
            timestamp: event.timestamp,
          })
          break

        case 'run_state': {
          if (event.state === 'running') {
            // Clear stale output from a previous run when this run begins emitting
            clearRunOutput()
          }
          // Invalidate runs list so RunsPanel reflects the new state immediately
          queryClient.invalidateQueries({ queryKey: runKeys.list(programId) })
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
                // Surface step errors in the output panel when the run failed
                // with no LLM tokens (e.g. missing API key, unconfigured connector).
                if (event.state === 'failed') {
                  const msgs = Object.values(errors)
                  if (msgs.length > 0) {
                    setRunError(msgs.join('\n'))
                  } else {
                    setRunError('Run failed — check the Logs tab for details')
                  }
                }
              })
              .catch(() => {
                if (event.state === 'failed') {
                  setRunError('Run failed — check the Logs tab for details')
                }
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
  }, [runId, programId, queryClient, setNodeStepState, setNodeStepErrors, clearRunState, setActiveRunId, appendLlmToken, appendLogEntry, clearRunOutput, setRunError])
}
