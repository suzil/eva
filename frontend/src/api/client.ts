import type {
  Program,
  Graph,
  CreateProgramReq,
  PatchProgramReq,
  ValidateResult,
  ApiError,
  Run,
  RunDetail,
  Credential,
  CreateCredentialReq,
} from '../types/index.ts'

const BASE = '/api'

async function request<T>(path: string, init?: RequestInit): Promise<T> {
  const res = await fetch(`${BASE}${path}`, {
    headers: { 'Content-Type': 'application/json', ...init?.headers },
    ...init,
  })
  if (!res.ok) {
    let message = `HTTP ${res.status}`
    try {
      const err = (await res.json()) as ApiError
      message = err.error
    } catch {
      // use default message
    }
    throw new Error(message)
  }
  // 204 No Content has no body
  if (res.status === 204) return undefined as T
  return res.json() as Promise<T>
}

// ---------------------------------------------------------------------------
// Programs
// ---------------------------------------------------------------------------

export function fetchPrograms(): Promise<Program[]> {
  return request<Program[]>('/programs')
}

export function fetchProgram(id: string): Promise<Program> {
  return request<Program>(`/programs/${id}`)
}

export function createProgram(body: CreateProgramReq): Promise<Program> {
  return request<Program>('/programs', {
    method: 'POST',
    body: JSON.stringify(body),
  })
}

export function patchProgram(id: string, body: PatchProgramReq): Promise<Program> {
  return request<Program>(`/programs/${id}`, {
    method: 'PATCH',
    body: JSON.stringify(body),
  })
}

export function deleteProgram(id: string): Promise<void> {
  return request<void>(`/programs/${id}`, { method: 'DELETE' })
}

export function putGraph(id: string, graph: Graph): Promise<Program> {
  return request<Program>(`/programs/${id}/graph`, {
    method: 'PUT',
    body: JSON.stringify(graph),
  })
}

export function validateProgram(id: string): Promise<ValidateResult> {
  return request<ValidateResult>(`/programs/${id}/validate`, { method: 'POST' })
}

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

export function fetchRuns(programId: string, limit = 20): Promise<Run[]> {
  return request<Run[]>(`/programs/${programId}/runs?limit=${limit}`)
}

export function createRun(programId: string, triggerPayload?: unknown): Promise<Run> {
  return request<Run>(`/programs/${programId}/runs`, {
    method: 'POST',
    body: JSON.stringify({ triggerPayload: triggerPayload ?? null }),
  })
}

export function fetchRunDetail(runId: string): Promise<RunDetail> {
  return request<RunDetail>(`/runs/${runId}`)
}

export function cancelRun(runId: string): Promise<Run> {
  return request<Run>(`/runs/${runId}/cancel`, { method: 'POST' })
}

// ---------------------------------------------------------------------------
// Credentials (EVA-32)
// ---------------------------------------------------------------------------

export function fetchCredentials(): Promise<Credential[]> {
  return request<Credential[]>('/credentials')
}

export function createCredential(body: CreateCredentialReq): Promise<Credential> {
  return request<Credential>('/credentials', {
    method: 'POST',
    body: JSON.stringify(body),
  })
}

export function deleteCredential(id: string): Promise<void> {
  return request<void>(`/credentials/${id}`, { method: 'DELETE' })
}
