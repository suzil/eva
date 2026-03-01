import { render } from '@testing-library/react'
import { describe, it } from 'vitest'
import { axe } from 'jest-axe'
import { SyncWarningModal } from '../SyncWarningModal'

describe('SyncWarningModal accessibility', () => {
  it('passes axe', async () => {
    const { container } = render(
      <SyncWarningModal onReplace={() => {}} onKeep={() => {}} />,
    )
    const results = await axe(container)
    expect(results).toHaveNoViolations()
  })
})
