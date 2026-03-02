/**
 * Direct REST client for e2e test setup and teardown.
 *
 * Calls the backend directly on port 8080 (bypassing Vite) so setup/teardown
 * is fast and independent of browser state.
 */

const API_URL = process.env.API_URL ?? 'http://localhost:8080'
const BASE = `${API_URL}/api`

// ---------------------------------------------------------------------------
// Types (minimal subset needed for test helpers)
// ---------------------------------------------------------------------------

export interface Program {
  id: string
  name: string
  state: 'draft' | 'active' | 'paused' | 'archived'
  createdAt: string
  updatedAt: string
}

export interface Graph {
  nodes: Record<string, GraphNode>
  edges: GraphEdge[]
}

export interface GraphNode {
  id: string
  label: string
  type: unknown
  posX: number
  posY: number
}

export interface GraphEdge {
  id: string
  sourceNode: string
  sourcePort: string
  targetNode: string
  targetPort: string
  category: 'data' | 'resource'
}

export interface Run {
  id: string
  programId: string
  state: 'pending' | 'running' | 'waiting' | 'completed' | 'failed' | 'canceled'
  startedAt?: string
  finishedAt?: string
}

// ---------------------------------------------------------------------------
// Test graphs
// ---------------------------------------------------------------------------

/**
 * Minimal Trigger → Agent graph using the default (no-key) OpenAI path.
 * Runs fail immediately without an API key — suitable for tests that don't
 * start a run, or that only need the run to *start* (not stay in-progress).
 */
export function makeTriggerAgentGraph(): Graph {
  // Use random UUIDs so that insertKey in the backend never collides across runs
  const triggerId = crypto.randomUUID()
  const agentId = crypto.randomUUID()
  const edgeId = crypto.randomUUID()
  return {
    nodes: {
      [triggerId]: {
        id: triggerId,
        label: 'Trigger',
        posX: 100,
        posY: 200,
        type: {
          type: 'trigger',
          config: {
            type: 'manual',
          },
        },
      },
      [agentId]: {
        id: agentId,
        label: 'Agent',
        posX: 400,
        posY: 200,
        type: {
          type: 'agent',
          config: {
            model: 'gpt-4o-mini',
            systemPrompt: 'You are a helpful assistant.',
            responseFormat: 'text',
            temperature: 0.7,
            maxIterations: 3,
          },
        },
      },
    },
    edges: [
      {
        id: edgeId,
        sourceNode: triggerId,
        sourcePort: 'event',
        targetNode: agentId,
        targetPort: 'instruction',
        category: 'data',
      },
    ],
  }
}

/**
 * Trigger → Agent graph configured to use Anthropic (claude-3-5-haiku-20241022).
 *
 * Used when EVA_ANTHROPIC_API_KEY is set so the backend's envAnthropicClient
 * (initialized at startup from that env var) handles the LLM call.  The run
 * then takes a few seconds instead of failing in <100 ms, which makes timing-
 * sensitive tests (Cancel button, etc.) reliable.
 *
 * Prompt and iterations are intentionally minimal to keep cost and latency low.
 */
export function makeTriggerAgentGraphForAnthropic(): Graph {
  const triggerId = crypto.randomUUID()
  const agentId = crypto.randomUUID()
  const edgeId = crypto.randomUUID()
  return {
    nodes: {
      [triggerId]: {
        id: triggerId,
        label: 'Trigger',
        posX: 100,
        posY: 200,
        type: {
          type: 'trigger',
          config: { type: 'manual' },
        },
      },
      [agentId]: {
        id: agentId,
        label: 'Agent',
        posX: 400,
        posY: 200,
        type: {
          type: 'agent',
          config: {
            provider: 'anthropic',
            model: 'claude-3-5-haiku-20241022',
            systemPrompt: 'Reply with exactly one short sentence.',
            responseFormat: 'text',
            temperature: 0.3,
            maxIterations: 1,
          },
        },
      },
    },
    edges: [
      {
        id: edgeId,
        sourceNode: triggerId,
        sourcePort: 'event',
        targetNode: agentId,
        targetPort: 'instruction',
        category: 'data',
      },
    ],
  }
}

// ---------------------------------------------------------------------------
// Low-level fetch wrapper
// ---------------------------------------------------------------------------

async function request<T>(path: string, init?: RequestInit): Promise<T> {
  const res = await fetch(`${BASE}${path}`, {
    headers: { 'Content-Type': 'application/json', ...init?.headers },
    ...init,
  })
  if (!res.ok) {
    let message = `HTTP ${res.status} ${res.statusText}`
    try {
      const body = await res.json() as { error?: string }
      if (body.error) message = body.error
    } catch {
      // use default message
    }
    throw new Error(`API ${path} → ${message}`)
  }
  if (res.status === 204) return undefined as T
  return res.json() as Promise<T>
}

// ---------------------------------------------------------------------------
// Programs
// ---------------------------------------------------------------------------

export function healthCheck(): Promise<{ status: string }> {
  return request<{ status: string }>('/health')
}

export function listPrograms(): Promise<Program[]> {
  return request<Program[]>('/programs')
}

export function createProgram(name: string): Promise<Program> {
  return request<Program>('/programs', {
    method: 'POST',
    body: JSON.stringify({ name }),
  })
}

export function getProgram(id: string): Promise<Program> {
  return request<Program>(`/programs/${id}`)
}

export function patchProgram(id: string, patch: { name?: string }): Promise<Program> {
  return request<Program>(`/programs/${id}`, {
    method: 'PATCH',
    body: JSON.stringify(patch),
  })
}

export function deleteProgram(id: string): Promise<void> {
  return request<void>(`/programs/${id}`, { method: 'DELETE' })
}

// ---------------------------------------------------------------------------
// Graph
// ---------------------------------------------------------------------------

export function saveGraph(programId: string, graph: Graph): Promise<void> {
  return request<void>(`/programs/${programId}/graph`, {
    method: 'PUT',
    body: JSON.stringify(graph),
  })
}

export function getSpec(programId: string): Promise<{ yaml: string }> {
  return request<{ yaml: string }>(`/programs/${programId}/spec`)
}

// ---------------------------------------------------------------------------
// Lifecycle
// ---------------------------------------------------------------------------

export function deployProgram(id: string): Promise<Program> {
  return request<Program>(`/programs/${id}/deploy`, { method: 'POST' })
}

export function pauseProgram(id: string): Promise<Program> {
  return request<Program>(`/programs/${id}/pause`, { method: 'POST' })
}

export function resumeProgram(id: string): Promise<Program> {
  return request<Program>(`/programs/${id}/resume`, { method: 'POST' })
}

// ---------------------------------------------------------------------------
// Runs
// ---------------------------------------------------------------------------

export function createRun(programId: string): Promise<Run> {
  return request<Run>(`/programs/${programId}/runs`, {
    method: 'POST',
    body: JSON.stringify({ triggerPayload: null }),
  })
}

export function cancelRun(runId: string): Promise<void> {
  return request<void>(`/runs/${runId}/cancel`, { method: 'POST' })
}

export function getRun(runId: string): Promise<Run> {
  return request<Run>(`/runs/${runId}`)
}

// ---------------------------------------------------------------------------
// Knowledge
// ---------------------------------------------------------------------------

export function createKnowledgeEntry(programId: string, title: string, content: string): Promise<{ id: string }> {
  return request<{ id: string }>(`/programs/${programId}/knowledge`, {
    method: 'POST',
    body: JSON.stringify({ title, content, sourceType: 'manual' }),
  })
}

// ---------------------------------------------------------------------------
// Cleanup helpers
// ---------------------------------------------------------------------------

/**
 * Delete all programs whose name starts with "e2e-test-".
 * Safe to call before/after test runs to keep the DB clean.
 */
export async function cleanupTestPrograms(): Promise<void> {
  const programs = await listPrograms()
  const testPrograms = programs.filter((p) => p.name.startsWith('e2e-test-'))
  await Promise.all(testPrograms.map((p) => deleteProgram(p.id)))
}
