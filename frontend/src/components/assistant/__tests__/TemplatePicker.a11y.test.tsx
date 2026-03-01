import { render } from '@testing-library/react'
import { describe, it, vi } from 'vitest'
import { axe } from 'jest-axe'
import { TemplatePicker } from '../TemplatePicker'

// ---------------------------------------------------------------------------
// Mock child components and API hook to avoid QueryClient / heavy deps
// ---------------------------------------------------------------------------

vi.mock('../../../api/hooks', () => ({
  useTemplates: () => ({
    data: [
      {
        id: 'tpl-1',
        name: 'Code Reviewer',
        description: 'Reviews code for issues',
        category: 'reviewer',
        tags: ['code', 'review'],
        body: 'Review this code: {{code}}',
        isBuiltIn: true,
        variables: [{ name: 'code', description: 'The code to review', required: true }],
        createdAt: '',
        updatedAt: '',
      },
    ],
    isLoading: false,
  }),
}))

vi.mock('../TemplateRow', () => ({
  TemplateRow: ({
    template,
    isSelected,
    onSelect,
  }: {
    template: { id: string; name: string }
    isSelected: boolean
    onSelect: () => void
  }) => (
    <div
      role="option"
      aria-selected={isSelected}
      onClick={onSelect}
      tabIndex={0}
      onKeyDown={(e) => e.key === 'Enter' && onSelect()}
    >
      {template.name}
    </div>
  ),
}))

vi.mock('../TemplatePreview', () => ({
  TemplatePreview: () => <section aria-label="Template preview" />,
}))

vi.mock('../TemplateEditor', () => ({
  TemplateEditor: () => <div />,
}))

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('TemplatePicker accessibility', () => {
  it('passes axe when open', async () => {
    const { container } = render(
      <TemplatePicker open onClose={() => {}} onInsert={() => {}} />,
    )
    const results = await axe(container)
    expect(results).toHaveNoViolations()
  })

  it('renders nothing when closed', () => {
    const { container } = render(
      <TemplatePicker open={false} onClose={() => {}} onInsert={() => {}} />,
    )
    expect(container.firstChild).toBeNull()
  })
})
