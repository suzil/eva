import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, beforeEach, vi } from 'vitest'
import React from 'react'
import { EditorTabs } from './EditorTabs'
import { useUiStore } from '../../store/uiStore'

// ---------------------------------------------------------------------------
// Mock heavyweight sub-components to isolate sync logic
// ---------------------------------------------------------------------------

vi.mock('./ContentArea', () => ({
  ContentArea: () => <div data-testid="content-area" />,
}))

vi.mock('../editor/SpecEditorView', () => ({
  SpecEditorView: () => <div data-testid="spec-editor-view" />,
}))

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const INITIAL_UI_STATE = {
  activeEditorTab: 'graph' as const,
  specSyncState: 'graph_source' as const,
  specDirty: false,
}

function renderTabs() {
  return render(<EditorTabs />)
}

function getSpecSyncState() {
  return useUiStore.getState().specSyncState
}

beforeEach(() => {
  useUiStore.setState(INITIAL_UI_STATE)
})

// ---------------------------------------------------------------------------
// T1: GRAPH → SPEC tab switch
// ---------------------------------------------------------------------------

describe('T1: switching to Spec tab', () => {
  it('sets specSyncState to yaml_source when coming from graph_source', () => {
    useUiStore.setState({ activeEditorTab: 'graph', specSyncState: 'graph_source' })
    renderTabs()

    fireEvent.click(screen.getByRole('button', { name: /spec/i }))

    expect(getSpecSyncState()).toBe('yaml_source')
    expect(useUiStore.getState().activeEditorTab).toBe('spec')
  })

  it('does not override conflict state when returning to Spec tab', () => {
    useUiStore.setState({ activeEditorTab: 'graph', specSyncState: 'conflict' })
    renderTabs()

    fireEvent.click(screen.getByRole('button', { name: /spec/i }))

    expect(getSpecSyncState()).toBe('conflict')
    expect(useUiStore.getState().activeEditorTab).toBe('spec')
  })

  it('renders SpecEditorView when Spec tab is active', () => {
    useUiStore.setState({ activeEditorTab: 'graph', specSyncState: 'graph_source' })
    renderTabs()

    fireEvent.click(screen.getByRole('button', { name: /spec/i }))

    expect(screen.getByTestId('spec-editor-view')).toBeTruthy()
  })
})

// ---------------------------------------------------------------------------
// T2: SPEC → GRAPH tab switch with dirty YAML
// ---------------------------------------------------------------------------

describe('T2: switching away from Spec tab with dirty YAML', () => {
  it('sets specSyncState to conflict when specDirty is true', () => {
    useUiStore.setState({ activeEditorTab: 'spec', specSyncState: 'yaml_source', specDirty: true })
    renderTabs()

    fireEvent.click(screen.getByRole('button', { name: /graph/i }))

    expect(getSpecSyncState()).toBe('conflict')
    expect(useUiStore.getState().activeEditorTab).toBe('graph')
  })

  it('does not set conflict when specDirty is false', () => {
    useUiStore.setState({ activeEditorTab: 'spec', specSyncState: 'yaml_source', specDirty: false })
    renderTabs()

    fireEvent.click(screen.getByRole('button', { name: /graph/i }))

    expect(getSpecSyncState()).toBe('yaml_source')
    expect(useUiStore.getState().activeEditorTab).toBe('graph')
  })

  it('shows dirty dot on Spec tab when specDirty is true', () => {
    useUiStore.setState({ activeEditorTab: 'spec', specSyncState: 'yaml_source', specDirty: true })
    const { container } = renderTabs()

    // The dirty dot is a <span> with rounded-full inside the Spec button
    const specButton = screen.getByRole('button', { name: /spec/i })
    const dot = specButton.querySelector('span.rounded-full')
    expect(dot).toBeTruthy()
    void container
  })

  it('does not show dirty dot when specDirty is false', () => {
    useUiStore.setState({ activeEditorTab: 'spec', specSyncState: 'yaml_source', specDirty: false })
    renderTabs()

    const specButton = screen.getByRole('button', { name: /spec/i })
    const dot = specButton.querySelector('span.rounded-full')
    expect(dot).toBeNull()
  })
})

// ---------------------------------------------------------------------------
// No-data-loss scenario: edit YAML → switch to Graph → make graph edit → switch back to Spec
// ---------------------------------------------------------------------------

describe('no-data-loss: conflict persists when returning to Spec after graph edits', () => {
  it('conflict state survives a round-trip through the Graph tab', () => {
    // 1. User edits YAML, then leaves Spec tab
    useUiStore.setState({ activeEditorTab: 'spec', specSyncState: 'yaml_source', specDirty: true })
    const { unmount } = renderTabs()

    // 2. Click Graph tab — conflict is flagged
    fireEvent.click(screen.getByRole('button', { name: /graph/i }))
    expect(getSpecSyncState()).toBe('conflict')
    expect(useUiStore.getState().activeEditorTab).toBe('graph')

    // 3. (Simulated) user makes a graph edit — canvasStore.isDirty would be true in prod
    // State machine in EditorTabs doesn't depend on canvasStore; conflict already set.

    // 4. User switches back to Spec tab — conflict must be preserved (no silent overwrite)
    fireEvent.click(screen.getByRole('button', { name: /spec/i }))
    expect(getSpecSyncState()).toBe('conflict')
    expect(useUiStore.getState().activeEditorTab).toBe('spec')

    unmount()
  })
})

// ---------------------------------------------------------------------------
// T3 / T4: Replace / Keep via SyncWarningModal
// Tests exercise the modal callbacks directly (Monaco mocked out above)
// ---------------------------------------------------------------------------

import { SyncWarningModal } from '../editor/SyncWarningModal'

describe('T3: Replace clears conflict and resets to graph_source', () => {
  it('calls onReplace which sets specSyncState to graph_source', () => {
    useUiStore.setState({ specSyncState: 'conflict' })

    const handleReplace = vi.fn(() => {
      useUiStore.getState().setSpecSyncState('graph_source')
    })
    const handleKeep = vi.fn()

    render(<SyncWarningModal onReplace={handleReplace} onKeep={handleKeep} />)
    fireEvent.click(screen.getByRole('button', { name: /replace graph/i }))

    expect(handleReplace).toHaveBeenCalledOnce()
    expect(getSpecSyncState()).toBe('graph_source')
  })
})

describe('T4: Keep preserves YAML edits and sets yaml_source', () => {
  it('calls onKeep which sets specSyncState to yaml_source', () => {
    useUiStore.setState({ specSyncState: 'conflict' })

    const handleReplace = vi.fn()
    const handleKeep = vi.fn(() => {
      useUiStore.getState().setSpecSyncState('yaml_source')
    })

    render(<SyncWarningModal onReplace={handleReplace} onKeep={handleKeep} />)
    fireEvent.click(screen.getByRole('button', { name: /keep my edits/i }))

    expect(handleKeep).toHaveBeenCalledOnce()
    expect(getSpecSyncState()).toBe('yaml_source')
  })
})
