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
  const setActiveRunId = useUiStore((s) => s.setActiveRunId)
  const setInspectedRunId = useUiStore((s) => s.setInspectedRunId)
  const appendLlmToken = useUiStore((s) => s.appendLlmToken)
  const setLlmOutput = useUiStore((s) => s.setLlmOutput)
  const appendLogEntry = useUiStore((s) => s.appendLogEntry)
  const clearRunOutput = useUiStore((s) => s.clearRunOutput)
  const setRunError = useUiStore((s) => s.setRunError)
  const setActiveBottomTab = useUiStore((s) => s.setActiveBottomTab)

  useEffect(() => {
    if (!runId) return

    // WebSocket connects to /api/ws (same origin, upgrade via dev proxy or production server)
    const wsUrl = `${window.location.protocol === 'https:' ? 'wss' : 'ws'}://${window.location.host}/api/ws`
    const ws = new WebSocket(wsUrl)
    let closed = false
    // Maps tool_call_id → function name so result events can reference the function.
    const toolCallNames = new Map<string, string>()
    // Switch to Logs tab on the first tool call so the user sees connector activity.
    let switchedToLogs = false

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

        case 'tool_call': {
          if (!switchedToLogs) {
            setActiveBottomTab('logs')
            switchedToLogs = true
          }
          const d = event.data
          if (event.phase === 'invoke') {
            const fn = String(d.function ?? '')
            const id = String(d.tool_call_id ?? '')
            if (id) toolCallNames.set(id, fn)
            const args = d.arguments != null ? JSON.stringify(d.arguments).slice(0, 300) : '{}'
            appendLogEntry({
              stepId: event.nodeId,
              level: 'info',
              message: `→ ${fn} ${args}`,
              timestamp: event.timestamp,
            })
          } else {
            const id = String(d.tool_call_id ?? '')
            const fn = toolCallNames.get(id) ?? 'tool'
            const resultVal = d.result ?? ''
            const raw = typeof resultVal === 'string' ? resultVal : JSON.stringify(resultVal)
            const truncated = raw.length > 400 ? raw.slice(0, 400) + '…' : raw
            appendLogEntry({
              stepId: event.nodeId,
              level: 'debug',
              message: `← ${fn}: ${truncated}`,
              timestamp: event.timestamp,
            })
          }
          break
        }

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
            setActiveRunId(null)
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
                // Populate RQ cache so useRunDetail returns immediately when
                // inspectedRunId is set — avoids a second network round-trip.
                queryClient.setQueryData(runKeys.detail(event.runId), detail)
                // Extract the agent text output from the completed run's steps.
                // This ensures the Output panel shows content even if llm_token
                // events were missed (e.g. background tab, slow connection).
                const agentStep = detail.steps.find((s) => {
                  const out = s.output as { type?: string } | null | undefined
                  return out?.type === 'agent_output'
                })
                const agentPayload = agentStep
                  ? (agentStep.output as { type: string; payload?: unknown }).payload
                  : undefined
                if (typeof agentPayload === 'string' && agentPayload.length > 0) {
                  setLlmOutput(agentPayload)
                  setActiveBottomTab('output')
                }
                // Auto-select the finished run so its step states stay visible
                // on the canvas and it gets highlighted in RunsPanel.
                setInspectedRunId(event.runId)
              })
              .catch(() => {
                if (event.state === 'failed') {
                  setRunError('Run failed — check the Logs tab for details')
                }
              })
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
    }
  }, [runId, programId, queryClient, setNodeStepState, setNodeStepErrors, setActiveRunId, setInspectedRunId, appendLlmToken, appendLogEntry, clearRunOutput, setRunError, setActiveBottomTab])
}
