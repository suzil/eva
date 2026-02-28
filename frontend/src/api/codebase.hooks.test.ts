import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, waitFor } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import React from 'react'
import { useCodebases } from './hooks.ts'
import type { CodebaseMetadata } from '../types/index.ts'

// ---------------------------------------------------------------------------
// Mock API client — factory must not reference outer variables (vi.mock is hoisted)
// ---------------------------------------------------------------------------

vi.mock('./client.ts', () => ({
  fetchCodebases: vi.fn(),
}))

const mockCodebase: CodebaseMetadata = {
  id: 'cb-1',
  programId: 'prog-1',
  path: '/workspace/eva',
  languageStats: { hs: 42, ts: 20 },
  keyFiles: ['README.md', 'eva.cabal'],
  gitBranch: 'main',
  gitDirty: false,
  lastScannedAt: '2026-02-27T00:00:00Z',
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function wrapper({ children }: { children: React.ReactNode }) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } })
  return React.createElement(QueryClientProvider, { client }, children)
}

beforeEach(async () => {
  vi.clearAllMocks()
  const { fetchCodebases } = await import('./client.ts')
  vi.mocked(fetchCodebases).mockResolvedValue([mockCodebase])
})

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('useCodebases', () => {
  it('fetches and returns codebase data for a program', async () => {
    const { result } = renderHook(() => useCodebases('prog-1'), { wrapper })
    await waitFor(() => expect(result.current.isSuccess).toBe(true))
    expect(result.current.data).toHaveLength(1)
    expect(result.current.data![0].id).toBe('cb-1')
    expect(result.current.data![0].path).toBe('/workspace/eva')
    expect(result.current.data![0].languageStats).toEqual({ hs: 42, ts: 20 })
  })

  it('is disabled when programId is null', () => {
    const { result } = renderHook(() => useCodebases(null), { wrapper })
    expect(result.current.fetchStatus).toBe('idle')
    expect(result.current.data).toBeUndefined()
  })

  it('uses distinct query keys per programId', async () => {
    const { fetchCodebases } = await import('./client.ts')
    renderHook(() => useCodebases('prog-1'), { wrapper })
    renderHook(() => useCodebases('prog-2'), { wrapper })
    await waitFor(() => expect(vi.mocked(fetchCodebases).mock.calls.length).toBeGreaterThanOrEqual(2))
    expect(vi.mocked(fetchCodebases)).toHaveBeenCalledWith('prog-1')
    expect(vi.mocked(fetchCodebases)).toHaveBeenCalledWith('prog-2')
  })
})
