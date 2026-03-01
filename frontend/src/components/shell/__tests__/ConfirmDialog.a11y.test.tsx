import { render } from '@testing-library/react'
import { describe, it } from 'vitest'
import { axe } from 'jest-axe'
import { ConfirmDialog } from '../ConfirmDialog'

describe('ConfirmDialog accessibility', () => {
  it('passes axe when open', async () => {
    const { container } = render(
      <ConfirmDialog
        open
        title="Confirm Deploy"
        message="Are you sure you want to deploy this program to active state?"
        confirmLabel="Deploy"
        cancelLabel="Cancel"
        onConfirm={() => {}}
        onCancel={() => {}}
      />,
    )
    const results = await axe(container)
    expect(results).toHaveNoViolations()
  })

  it('renders nothing when closed', () => {
    const { container } = render(
      <ConfirmDialog
        open={false}
        title="Confirm"
        message="Are you sure?"
        onConfirm={() => {}}
        onCancel={() => {}}
      />,
    )
    // Closed dialog renders nothing — no axe check needed, just ensure it doesn't throw
    const results = container.firstChild
    expect(results).toBeNull()
  })
})
