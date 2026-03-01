import { render } from '@testing-library/react'
import { describe, it, vi, beforeEach } from 'vitest'
import { axe } from 'jest-axe'
import { useUiStore } from '../../../store/uiStore'
import { AssistantPanel } from '../AssistantPanel'

// ---------------------------------------------------------------------------
// Mock child components and hooks that pull in heavy dependencies
// ---------------------------------------------------------------------------

vi.mock('../MessageList', () => ({
  MessageList: () => <div role="log" aria-label="Messages" />,
}))

vi.mock('../AssistantInput', () => ({
  AssistantInput: ({ disabled }: { disabled?: boolean }) => (
    <textarea aria-label="Message MAGI" disabled={disabled} />
  ),
}))

vi.mock('../NodeReferenceChip', () => ({
  NodeReferenceChip: ({ label }: { label: string }) => <span>{label}</span>,
}))

vi.mock('../../../api/hooks', () => ({
  usePrograms: () => ({ data: [] }),
}))

vi.mock('../../../store/canvasStore', () => ({
  useCanvasStore: (selector: (s: { selectedNodeId: null; nodes: [] }) => unknown) =>
    selector({ selectedNodeId: null, nodes: [] }),
}))

vi.mock('../../../hooks/useAssistantStream', () => ({
  useAssistantStream: () => ({ sendMessage: vi.fn(), streamingText: '' }),
}))

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('AssistantPanel accessibility', () => {
  beforeEach(() => {
    useUiStore.setState({
      selectedProgramId: null,
      assistantConversations: {},
      prefillAssistantMessage: null,
      pendingAssistantMessage: null,
    })
  })

  it('passes axe in empty state (no program selected)', async () => {
    const { container } = render(<AssistantPanel />)
    const results = await axe(container)
    expect(results).toHaveNoViolations()
  })
})
