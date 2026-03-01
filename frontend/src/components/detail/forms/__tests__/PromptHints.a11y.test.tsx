import { render, fireEvent, screen } from '@testing-library/react'
import { describe, it, vi } from 'vitest'
import { axe } from 'jest-axe'
import { PromptHints } from '../PromptHints'

// ---------------------------------------------------------------------------
// Mock uiStore — PromptHints only uses action dispatchers, not state reads
// ---------------------------------------------------------------------------

vi.mock('../../../../store/uiStore', () => ({
  useUiStore: (selector: (s: object) => unknown) =>
    selector({
      setDetailPanelTab: vi.fn(),
      setPrefillAssistantMessage: vi.fn(),
    }),
}))

// A short prompt reliably triggers the "too brief" hint on the first render
// because debouncedPrompt is initialised to systemPrompt (no timer needed).
const SHORT_PROMPT = 'Do something'

function renderHints() {
  return render(
    <PromptHints
      nodeId="node-1"
      systemPrompt={SHORT_PROMPT}
      responseFormat="text"
      knowledgeLabels={[]}
      connectorLabels={[]}
    />,
  )
}

describe('PromptHints accessibility', () => {
  it('passes axe in collapsed state', async () => {
    const { container } = renderHints()
    const results = await axe(container)
    expect(results).toHaveNoViolations()
  })

  it('passes axe in expanded state', async () => {
    const { container } = renderHints()

    // Expand the hint list via the View/Hide toggle button
    const viewButton = screen.getByRole('button', { name: /view/i })
    fireEvent.click(viewButton)

    const results = await axe(container)
    expect(results).toHaveNoViolations()
  })
})
