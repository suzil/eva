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
  SpecResponse,
  CodebaseMetadata,
  ConnectCodebaseReq,
  FileNode,
  FileEntry,
  WriteFileReq,
  GitDiffResponse,
  CodeChangeset,
  FileChange,
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

// ---------------------------------------------------------------------------
// Spec (YAML declarative view) — EVA-62
// ---------------------------------------------------------------------------

export function fetchSpec(programId: string): Promise<SpecResponse> {
  return request<SpecResponse>(`/programs/${programId}/spec`)
}

/** Structured error thrown when PUT /spec returns 422. Carries the backend ParseError list. */
export class SpecSaveError extends Error {
  constructor(public readonly errors: Array<{ message: string }>) {
    super(errors.map((e) => e.message).join('; '))
    this.name = 'SpecSaveError'
  }
}

export async function putSpec(programId: string, yaml: string): Promise<Program> {
  const res = await fetch(`${BASE}/programs/${programId}/spec`, {
    method: 'PUT',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ yaml }),
  })
  if (!res.ok) {
    if (res.status === 422) {
      const body = (await res.json()) as { errors: Array<{ message: string }> }
      throw new SpecSaveError(body.errors ?? [])
    }
    throw new Error(`HTTP ${res.status}`)
  }
  return res.json() as Promise<Program>
}

// ---------------------------------------------------------------------------
// Codebase (P2-M4) — EVA-74
// ---------------------------------------------------------------------------

export function fetchCodebases(programId: string): Promise<CodebaseMetadata[]> {
  return request<CodebaseMetadata[]>(`/programs/${programId}/codebase`)
}

export function connectCodebase(programId: string, body: ConnectCodebaseReq): Promise<CodebaseMetadata> {
  return request<CodebaseMetadata>(`/programs/${programId}/codebase`, {
    method: 'POST',
    body: JSON.stringify(body),
  })
}

export function disconnectCodebase(programId: string, codebaseId: string): Promise<void> {
  return request<void>(`/programs/${programId}/codebase/${codebaseId}`, { method: 'DELETE' })
}

export function fetchFileTree(codebaseId: string): Promise<FileNode> {
  return request<FileNode>(`/codebase/${codebaseId}/tree`)
}

export function fetchFile(codebaseId: string, path: string): Promise<FileEntry> {
  return request<FileEntry>(`/codebase/${codebaseId}/file?path=${encodeURIComponent(path)}`)
}

export function writeFile(codebaseId: string, body: WriteFileReq): Promise<void> {
  return request<void>(`/codebase/${codebaseId}/file`, {
    method: 'PUT',
    body: JSON.stringify(body),
  })
}

export function fetchGitDiff(codebaseId: string): Promise<GitDiffResponse> {
  return request<GitDiffResponse>(`/codebase/${codebaseId}/diff`)
}

export function refreshCodebase(codebaseId: string): Promise<CodebaseMetadata> {
  return request<CodebaseMetadata>(`/codebase/${codebaseId}/refresh`, { method: 'POST' })
}

// ---------------------------------------------------------------------------
// Changesets (P2-M4) — EVA-74
// ---------------------------------------------------------------------------

export function fetchProgramChangesets(programId: string): Promise<CodeChangeset[]> {
  return request<CodeChangeset[]>(`/programs/${programId}/changesets`)
}

export function fetchRunChangesets(runId: string): Promise<CodeChangeset[]> {
  return request<CodeChangeset[]>(`/runs/${runId}/changesets`)
}

export function fetchChangeset(id: string): Promise<CodeChangeset> {
  return request<CodeChangeset>(`/changesets/${id}`)
}

export function acceptFile(changesetId: string, fileId: string): Promise<FileChange> {
  return request<FileChange>(`/changesets/${changesetId}/files/${fileId}/accept`, { method: 'POST' })
}

export function rejectFile(changesetId: string, fileId: string): Promise<FileChange> {
  return request<FileChange>(`/changesets/${changesetId}/files/${fileId}/reject`, { method: 'POST' })
}

export function acceptAllChanges(changesetId: string): Promise<CodeChangeset> {
  return request<CodeChangeset>(`/changesets/${changesetId}/accept-all`, { method: 'POST' })
}

export function rejectAllChanges(changesetId: string): Promise<CodeChangeset> {
  return request<CodeChangeset>(`/changesets/${changesetId}/reject-all`, { method: 'POST' })
}
