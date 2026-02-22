import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, vi, beforeEach } from 'vitest'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import React from 'react'
import { Toolbar } from './Toolbar'
import { useCanvasStore } from '../../store/canvasStore'
import { useUiStore } from '../../store/uiStore'
import type { Run } from '../../types'

// ---------------------------------------------------------------------------
// Stable mock data (hoisted so vi.mock factory can reference them)
// ---------------------------------------------------------------------------

const hoisted = vi.hoisted(() => ({
  mockSave: vi.fn(),
  mockValidate: vi.fn(),
  mockCreateRun: vi.fn(),
  mockDeploy: vi.fn(),
  // Stable array reference — prevents [mode, runsData] effect from looping
  runsData: { data: [] as Run[] },
  programData: { data: { id: 'p1', name: 'Test Program', state: 'draft' as const } },
}))

// ---------------------------------------------------------------------------
// Mock api/hooks
// ---------------------------------------------------------------------------

vi.mock('../../api/hooks', () => ({
  useProgram: vi.fn(() => hoisted.programData),
  useSaveGraph: vi.fn(() => ({ mutate: hoisted.mockSave, isPending: false })),
  useValidateProgram: vi.fn(() => ({ mutate: hoisted.mockValidate })),
  useCreateRun: vi.fn(() => ({ mutate: hoisted.mockCreateRun })),
  useDeployProgram: vi.fn(() => ({ mutate: hoisted.mockDeploy })),
  useCancelRun: vi.fn(() => ({ mutate: vi.fn(), isPending: false })),
  usePauseProgram: vi.fn(() => ({ mutate: vi.fn(), isPending: false })),
  useResumeProgram: vi.fn(() => ({ mutate: vi.fn(), isPending: false })),
  useRuns: vi.fn(() => hoisted.runsData),
  useRunDetail: vi.fn(() => ({ data: undefined })),
}))

vi.mock('../../hooks/useRunStream', () => ({ useRunStream: vi.fn() }))

import * as apiHooks from '../../api/hooks'

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function wrapper({ children }: { children: React.ReactNode }) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } })
  return <QueryClientProvider client={client}>{children}</QueryClientProvider>
}

function renderToolbar() {
  return render(<Toolbar />, { wrapper })
}

beforeEach(() => {
  vi.clearAllMocks()
  // Restore defaults after per-test overrides
  vi.mocked(apiHooks.useProgram).mockReturnValue(hoisted.programData as ReturnType<typeof apiHooks.useProgram>)
  vi.mocked(apiHooks.useRuns).mockReturnValue(hoisted.runsData as ReturnType<typeof apiHooks.useRuns>)
  vi.mocked(apiHooks.useSaveGraph).mockReturnValue({ mutate: hoisted.mockSave, isPending: false } as ReturnType<typeof apiHooks.useSaveGraph>)
  vi.mocked(apiHooks.useValidateProgram).mockReturnValue({ mutate: hoisted.mockValidate } as ReturnType<typeof apiHooks.useValidateProgram>)
  vi.mocked(apiHooks.useCreateRun).mockReturnValue({ mutate: hoisted.mockCreateRun } as ReturnType<typeof apiHooks.useCreateRun>)
  vi.mocked(apiHooks.useDeployProgram).mockReturnValue({ mutate: hoisted.mockDeploy } as ReturnType<typeof apiHooks.useDeployProgram>)
  vi.mocked(apiHooks.useCancelRun).mockReturnValue({ mutate: vi.fn(), isPending: false } as ReturnType<typeof apiHooks.useCancelRun>)
  vi.mocked(apiHooks.usePauseProgram).mockReturnValue({ mutate: vi.fn(), isPending: false } as ReturnType<typeof apiHooks.usePauseProgram>)
  vi.mocked(apiHooks.useResumeProgram).mockReturnValue({ mutate: vi.fn(), isPending: false } as ReturnType<typeof apiHooks.useResumeProgram>)
  vi.mocked(apiHooks.useRunDetail).mockReturnValue({ data: undefined } as ReturnType<typeof apiHooks.useRunDetail>)

  useUiStore.setState({
    selectedProgramId: 'p1',
    mode: 'author',
    activeRunId: null,
    inspectedRunId: null,
    activeActivity: 'nodes',
  })
  useCanvasStore.setState({
    isDirty: true,
    nodes: [],
    edges: [],
    past: [],
    future: [],
    nodeStepStates: {},
    nodeStepErrors: {},
    currentProgramId: 'p1',
    selectedNodeId: null,
    selectedEdgeId: null,
    triggerFitView: false,
  })
})

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('Toolbar', () => {
  it('Save button is disabled when isDirty is false', () => {
    useCanvasStore.setState({ isDirty: false })
    renderToolbar()
    expect(screen.getByRole('button', { name: 'Save' })).toBeDisabled()
  })

  it('Save button is enabled and calls saveMutation.mutate when isDirty is true', () => {
    renderToolbar()
    const saveBtn = screen.getByRole('button', { name: 'Save' })
    expect(saveBtn).not.toBeDisabled()
    fireEvent.click(saveBtn)
    expect(hoisted.mockSave).toHaveBeenCalled()
  })

  it('Run button is visible for a draft program and calls validateMutation on click', () => {
    renderToolbar()
    const runBtn = screen.getByRole('button', { name: 'Run' })
    expect(runBtn).not.toBeDisabled()
    fireEvent.click(runBtn)
    expect(hoisted.mockValidate).toHaveBeenCalled()
  })

  it('Deploy button is visible for a draft program and calls validateMutation on click', () => {
    renderToolbar()
    const deployBtn = screen.getByRole('button', { name: 'Deploy' })
    expect(deployBtn).not.toBeDisabled()
    fireEvent.click(deployBtn)
    expect(hoisted.mockValidate).toHaveBeenCalled()
  })

  it('shows run error banner when validation onError is called', () => {
    vi.mocked(apiHooks.useValidateProgram).mockReturnValue({
      mutate: (_: unknown, opts: { onError?: (e: Error) => void }) => {
        opts?.onError?.(new Error('Missing required connection'))
      },
    } as ReturnType<typeof apiHooks.useValidateProgram>)
    renderToolbar()
    fireEvent.click(screen.getByRole('button', { name: 'Run' }))
    expect(screen.getByText(/Missing required connection/i)).toBeInTheDocument()
  })

  it('Deploy button is hidden and Pause button appears when program state is active', () => {
    const stableActiveData = { data: { id: 'p1', name: 'Test', state: 'active' as const } }
    vi.mocked(apiHooks.useProgram).mockReturnValue(stableActiveData as ReturnType<typeof apiHooks.useProgram>)
    renderToolbar()
    expect(screen.queryByRole('button', { name: 'Deploy' })).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Pause' })).toBeInTheDocument()
  })
})
