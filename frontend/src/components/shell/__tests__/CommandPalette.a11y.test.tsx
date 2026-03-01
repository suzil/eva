import { render } from '@testing-library/react'
import { describe, it, vi, beforeEach } from 'vitest'
import { axe } from 'jest-axe'
import { useUiStore } from '../../../store/uiStore'
import { CommandBar } from '../CommandPalette'

// ---------------------------------------------------------------------------
// Mock API hooks — avoid QueryClient requirement in unit tests
// ---------------------------------------------------------------------------

vi.mock('../../../api/hooks', () => ({
  usePrograms: () => ({ data: [] }),
  useProgram: () => ({ data: null }),
  useDeployProgram: () => ({ mutate: vi.fn() }),
  usePauseProgram: () => ({ mutate: vi.fn() }),
  useResumeProgram: () => ({ mutate: vi.fn() }),
  useCreateRun: () => ({ mutate: vi.fn() }),
}))

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('CommandBar accessibility', () => {
  beforeEach(() => {
    useUiStore.setState({
      commandBarOpen: true,
      selectedProgramId: null,
    })
  })

  it('passes axe when open with no programs', async () => {
    const { container } = render(<CommandBar />)
    const results = await axe(container)
    expect(results).toHaveNoViolations()
  })

  it('renders nothing when closed', () => {
    useUiStore.setState({ commandBarOpen: false })
    const { container } = render(<CommandBar />)
    expect(container.firstChild).toBeNull()
  })
})
