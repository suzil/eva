import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import type {
  Graph,
  CreateProgramReq,
  PatchProgramReq,
  CreateCredentialReq,
  ConnectCodebaseReq,
  WriteFileReq,
} from '../types/index.ts'
import {
  cancelRun,
  createCredential,
  createProgram,
  createRun,
  deleteCredential,
  deleteProgram,
  deployProgram,
  fetchCredentials,
  fetchProgram,
  fetchPrograms,
  fetchRunDetail,
  fetchRuns,
  fetchSpec,
  patchProgram,
  pauseProgram,
  putGraph,
  putSpec,
  resumeProgram,
  validateProgram,
  fetchCodebases,
  connectCodebase,
  disconnectCodebase,
  fetchFileTree,
  refreshCodebase,
  writeFile,
  fetchProgramChangesets,
  fetchRunChangesets,
  fetchChangeset,
  acceptFile,
  rejectFile,
  acceptAllChanges,
  rejectAllChanges,
} from './client.ts'

// ---------------------------------------------------------------------------
// Query key factory — centralises key shapes so invalidation is consistent
// ---------------------------------------------------------------------------

export const programKeys = {
  all: ['programs'] as const,
  detail: (id: string) => ['programs', id] as const,
}

// ---------------------------------------------------------------------------
// Queries
// ---------------------------------------------------------------------------

export function usePrograms() {
  return useQuery({
    queryKey: programKeys.all,
    queryFn: fetchPrograms,
  })
}

export function useProgram(id: string) {
  return useQuery({
    queryKey: programKeys.detail(id),
    queryFn: () => fetchProgram(id),
    enabled: Boolean(id),
  })
}

// ---------------------------------------------------------------------------
// Mutations
// ---------------------------------------------------------------------------

export function useCreateProgram() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (body: CreateProgramReq) => createProgram(body),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: programKeys.all })
    },
  })
}

export function usePatchProgram(id: string) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (body: PatchProgramReq) => patchProgram(id, body),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: programKeys.detail(id) })
      void queryClient.invalidateQueries({ queryKey: programKeys.all })
    },
  })
}

export function useDeleteProgram() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (id: string) => deleteProgram(id),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: programKeys.all })
    },
  })
}

/** Full-graph PUT — replaces the stored graph atomically. */
export function useSaveGraph(programId: string) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (graph: Graph) => putGraph(programId, graph),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: programKeys.detail(programId) })
    },
  })
}

export function useValidateProgram(id: string) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: () => validateProgram(id),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: programKeys.detail(id) })
    },
  })
}

export function useDeployProgram(id: string) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: () => deployProgram(id),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: programKeys.detail(id) })
      void queryClient.invalidateQueries({ queryKey: programKeys.all })
    },
  })
}

export function usePauseProgram(id: string) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: () => pauseProgram(id),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: programKeys.detail(id) })
    },
  })
}

export function useResumeProgram(id: string) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: () => resumeProgram(id),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: programKeys.detail(id) })
    },
  })
}

// ---------------------------------------------------------------------------
// Runs
// ---------------------------------------------------------------------------

export const runKeys = {
  list: (programId: string) => ['runs', 'list', programId] as const,
  detail: (id: string) => ['runs', id] as const,
}

export function useRuns(programId: string | null) {
  return useQuery({
    queryKey: runKeys.list(programId ?? ''),
    queryFn: () => fetchRuns(programId!),
    enabled: Boolean(programId),
  })
}

export function useCreateRun(programId: string) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (triggerPayload?: unknown) => createRun(programId, triggerPayload),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: runKeys.list(programId) })
    },
  })
}

export function useRunDetail(runId: string | null) {
  return useQuery({
    queryKey: runKeys.detail(runId ?? ''),
    queryFn: () => fetchRunDetail(runId!),
    enabled: Boolean(runId),
  })
}

export function useCancelRun() {
  return useMutation({
    mutationFn: (runId: string) => cancelRun(runId),
  })
}

// ---------------------------------------------------------------------------
// Credentials (EVA-32)
// ---------------------------------------------------------------------------

export const credentialKeys = {
  all: ['credentials'] as const,
}

export function useCredentials() {
  return useQuery({
    queryKey: credentialKeys.all,
    queryFn: fetchCredentials,
  })
}

export function useCreateCredential() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (body: CreateCredentialReq) => createCredential(body),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: credentialKeys.all })
    },
  })
}

export function useDeleteCredential() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (id: string) => deleteCredential(id),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: credentialKeys.all })
    },
  })
}

// ---------------------------------------------------------------------------
// Spec (YAML declarative view) — EVA-62
// ---------------------------------------------------------------------------

export const specKeys = {
  detail: (programId: string) => ['spec', programId] as const,
}

export function useSpec(programId: string | null) {
  return useQuery({
    queryKey: specKeys.detail(programId ?? ''),
    queryFn: () => fetchSpec(programId!),
    enabled: Boolean(programId),
  })
}

export function useSaveSpec(programId: string) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (yaml: string) => putSpec(programId, yaml),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: programKeys.detail(programId) })
      void queryClient.invalidateQueries({ queryKey: specKeys.detail(programId) })
    },
  })
}

// ---------------------------------------------------------------------------
// WebSocket invalidation hook point
// EVA-27: call invalidateGraph() when WS run_state / step_state events arrive
// to force a fresh server fetch of the program (including updated graph state).
// ---------------------------------------------------------------------------

export function useGraphInvalidation(programId: string) {
  const queryClient = useQueryClient()
  const invalidateGraph = () => {
    void queryClient.invalidateQueries({ queryKey: programKeys.detail(programId) })
  }
  return { invalidateGraph }
}

// ---------------------------------------------------------------------------
// Codebase (P2-M4) — EVA-74
// ---------------------------------------------------------------------------

export const codebaseKeys = {
  list: (programId: string) => ['codebases', 'list', programId] as const,
  tree: (codebaseId: string) => ['codebases', 'tree', codebaseId] as const,
  diff: (codebaseId: string) => ['codebases', 'diff', codebaseId] as const,
}

export const changesetKeys = {
  byProgram: (programId: string) => ['changesets', 'program', programId] as const,
  byRun: (runId: string) => ['changesets', 'run', runId] as const,
  detail: (id: string) => ['changesets', id] as const,
}

export function useCodebases(programId: string | null) {
  return useQuery({
    queryKey: codebaseKeys.list(programId ?? ''),
    queryFn: () => fetchCodebases(programId!),
    enabled: Boolean(programId),
  })
}

export function useFileTree(codebaseId: string | null) {
  return useQuery({
    queryKey: codebaseKeys.tree(codebaseId ?? ''),
    queryFn: () => fetchFileTree(codebaseId!),
    enabled: Boolean(codebaseId),
  })
}

export function useProgramChangesets(programId: string | null) {
  return useQuery({
    queryKey: changesetKeys.byProgram(programId ?? ''),
    queryFn: () => fetchProgramChangesets(programId!),
    enabled: Boolean(programId),
  })
}

export function useRunChangesets(runId: string | null) {
  return useQuery({
    queryKey: changesetKeys.byRun(runId ?? ''),
    queryFn: () => fetchRunChangesets(runId!),
    enabled: Boolean(runId),
  })
}

export function useChangeset(id: string | null) {
  return useQuery({
    queryKey: changesetKeys.detail(id ?? ''),
    queryFn: () => fetchChangeset(id!),
    enabled: Boolean(id),
  })
}

export function useConnectCodebase(programId: string) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (body: ConnectCodebaseReq) => connectCodebase(programId, body),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: codebaseKeys.list(programId) })
    },
  })
}

export function useDisconnectCodebase(programId: string) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (codebaseId: string) => disconnectCodebase(programId, codebaseId),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: codebaseKeys.list(programId) })
    },
  })
}

export function useWriteFile(codebaseId: string) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (body: WriteFileReq) => writeFile(codebaseId, body),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: codebaseKeys.diff(codebaseId) })
    },
  })
}

export function useRefreshCodebase() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (codebaseId: string) => refreshCodebase(codebaseId),
    onSuccess: (_data, codebaseId) => {
      void queryClient.invalidateQueries({ queryKey: codebaseKeys.tree(codebaseId) })
    },
  })
}

export function useAcceptFile() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: ({ changesetId, fileId }: { changesetId: string; fileId: string }) =>
      acceptFile(changesetId, fileId),
    onSuccess: (_data, { changesetId }) => {
      void queryClient.invalidateQueries({ queryKey: changesetKeys.detail(changesetId) })
    },
  })
}

export function useRejectFile() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: ({ changesetId, fileId }: { changesetId: string; fileId: string }) =>
      rejectFile(changesetId, fileId),
    onSuccess: (_data, { changesetId }) => {
      void queryClient.invalidateQueries({ queryKey: changesetKeys.detail(changesetId) })
    },
  })
}

export function useAcceptAll() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (changesetId: string) => acceptAllChanges(changesetId),
    onSuccess: (_data, changesetId) => {
      void queryClient.invalidateQueries({ queryKey: changesetKeys.detail(changesetId) })
    },
  })
}

export function useRejectAll() {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: (changesetId: string) => rejectAllChanges(changesetId),
    onSuccess: (_data, changesetId) => {
      void queryClient.invalidateQueries({ queryKey: changesetKeys.detail(changesetId) })
    },
  })
}
