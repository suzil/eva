import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, vi, beforeEach } from 'vitest'
import { ConnectorForm } from './ConnectorForm'
import type { ConnectorConfig, Credential } from '../../../types'

// ---------------------------------------------------------------------------
// Mock useCredentials
// ---------------------------------------------------------------------------

vi.mock('../../../api/hooks', () => ({
  useCredentials: vi.fn(),
}))

import { useCredentials } from '../../../api/hooks'

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const LINEAR_CREDENTIAL: Credential = {
  id: 'cred-1',
  name: 'Linear API Key',
  system: 'linear',
  type: 'api_key',
  createdAt: '2026-01-01T00:00:00Z',
}

const BASE_CONFIG: ConnectorConfig = {
  system: 'linear',
  credentialId: undefined,
  endpoint: undefined,
  scope: undefined,
  actionFilter: [],
}

function renderForm(config: ConnectorConfig = BASE_CONFIG, onChange = vi.fn()) {
  return { onChange, ...render(<ConnectorForm config={config} onChange={onChange} />) }
}

beforeEach(() => {
  vi.mocked(useCredentials).mockReturnValue({ data: [] } as unknown as ReturnType<typeof useCredentials>)
})

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('ConnectorForm', () => {
  it('renders all 4 system type options', () => {
    renderForm()
    const select = screen.getByDisplayValue('Linear')
    expect(select).toBeInTheDocument()
    expect(screen.getByRole('option', { name: 'GitHub' })).toBeInTheDocument()
    expect(screen.getByRole('option', { name: 'HTTP / REST' })).toBeInTheDocument()
    expect(screen.getByRole('option', { name: 'Codebase' })).toBeInTheDocument()
  })

  it('shows "no credentials" empty state when useCredentials returns empty', () => {
    vi.mocked(useCredentials).mockReturnValue({ data: [] } as unknown as ReturnType<typeof useCredentials>)
    renderForm()
    expect(screen.getByText(/No credentials for this system/i)).toBeInTheDocument()
  })

  it('shows credential select when credentials exist for the current system', () => {
    vi.mocked(useCredentials).mockReturnValue({ data: [LINEAR_CREDENTIAL] } as unknown as ReturnType<typeof useCredentials>)
    renderForm()
    expect(screen.getByDisplayValue('— select credential —')).toBeInTheDocument()
    expect(screen.getByRole('option', { name: 'Linear API Key' })).toBeInTheDocument()
  })

  it('shows warning when credentials exist but none is selected', () => {
    vi.mocked(useCredentials).mockReturnValue({ data: [LINEAR_CREDENTIAL] } as unknown as ReturnType<typeof useCredentials>)
    renderForm({ ...BASE_CONFIG, credentialId: undefined })
    expect(screen.getByText(/No credential selected/i)).toBeInTheDocument()
  })

  it('changing system calls onChange with new system and credentialId cleared', () => {
    vi.mocked(useCredentials).mockReturnValue({ data: [LINEAR_CREDENTIAL] } as unknown as ReturnType<typeof useCredentials>)
    const { onChange } = renderForm({ ...BASE_CONFIG, credentialId: 'cred-1' })
    const systemSelect = screen.getByDisplayValue('Linear')
    fireEvent.change(systemSelect, { target: { value: 'github' } })
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ system: 'github', credentialId: undefined }),
    )
  })

  it('entering endpoint text calls onChange with endpoint value', () => {
    const { onChange } = renderForm()
    const endpointInput = screen.getByPlaceholderText('https://…')
    fireEvent.change(endpointInput, { target: { value: 'https://api.example.com' } })
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ endpoint: 'https://api.example.com' }),
    )
  })

  // ---------------------------------------------------------------------------
  // Action filter
  // ---------------------------------------------------------------------------

  it('shows Actions section for linear system', () => {
    renderForm()
    expect(screen.getByText('Actions')).toBeInTheDocument()
    expect(screen.getByLabelText('List Issues')).toBeInTheDocument()
    expect(screen.getByLabelText('Create Issue')).toBeInTheDocument()
    expect(screen.getByLabelText('Update Issue')).toBeInTheDocument()
  })

  it('does not show Actions section for github system', () => {
    renderForm({ ...BASE_CONFIG, system: 'github' })
    expect(screen.queryByText('Actions')).not.toBeInTheDocument()
  })

  it('does not show Actions section for http system', () => {
    renderForm({ ...BASE_CONFIG, system: 'http' })
    expect(screen.queryByText('Actions')).not.toBeInTheDocument()
  })

  it('shows Actions section with codebase actions for codebase system', () => {
    renderForm({ ...BASE_CONFIG, system: 'codebase' })
    expect(screen.getByLabelText('List Tree')).toBeInTheDocument()
    expect(screen.getByLabelText('Read File')).toBeInTheDocument()
    expect(screen.getByLabelText('Git Diff')).toBeInTheDocument()
    expect(screen.getByLabelText('Write File')).toBeInTheDocument()
  })

  it('checking an action adds it to actionFilter', () => {
    const { onChange } = renderForm({ ...BASE_CONFIG, actionFilter: [] })
    fireEvent.click(screen.getByLabelText('List Issues'))
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ actionFilter: ['list_issues'] }),
    )
  })

  it('unchecking an action removes it from actionFilter', () => {
    const { onChange } = renderForm({ ...BASE_CONFIG, actionFilter: ['list_issues', 'create_issue'] })
    fireEvent.click(screen.getByLabelText('List Issues'))
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ actionFilter: ['create_issue'] }),
    )
  })

  it('changing system resets actionFilter to []', () => {
    vi.mocked(useCredentials).mockReturnValue({ data: [LINEAR_CREDENTIAL] } as unknown as ReturnType<typeof useCredentials>)
    const { onChange } = renderForm({ ...BASE_CONFIG, credentialId: 'cred-1', actionFilter: ['list_issues'] })
    const systemSelect = screen.getByDisplayValue('Linear')
    fireEvent.change(systemSelect, { target: { value: 'codebase' } })
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ system: 'codebase', credentialId: undefined, actionFilter: [] }),
    )
  })

  it('action checkboxes reflect current actionFilter state', () => {
    renderForm({ ...BASE_CONFIG, actionFilter: ['create_issue'] })
    expect(screen.getByLabelText('List Issues')).not.toBeChecked()
    expect(screen.getByLabelText('Create Issue')).toBeChecked()
    expect(screen.getByLabelText('Update Issue')).not.toBeChecked()
  })
})
