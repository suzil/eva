import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import type { Graph, CreateProgramReq, PatchProgramReq } from '../types/index.ts'
import {
  cancelRun,
  createProgram,
  createRun,
  deleteProgram,
  deployProgram,
  fetchProgram,
  fetchPrograms,
  fetchRunDetail,
  patchProgram,
  pauseProgram,
  putGraph,
  resumeProgram,
  validateProgram,
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
  detail: (id: string) => ['runs', id] as const,
}

export function useCreateRun(programId: string) {
  return useMutation({
    mutationFn: (triggerPayload?: unknown) => createRun(programId, triggerPayload),
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
