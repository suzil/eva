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
  vi.mocked(useCredentials).mockReturnValue({ data: [] } as ReturnType<typeof useCredentials>)
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
    vi.mocked(useCredentials).mockReturnValue({ data: [] } as ReturnType<typeof useCredentials>)
    renderForm()
    expect(screen.getByText(/No credentials for this system/i)).toBeInTheDocument()
  })

  it('shows credential select when credentials exist for the current system', () => {
    vi.mocked(useCredentials).mockReturnValue({ data: [LINEAR_CREDENTIAL] } as ReturnType<typeof useCredentials>)
    renderForm()
    expect(screen.getByDisplayValue('— select credential —')).toBeInTheDocument()
    expect(screen.getByRole('option', { name: 'Linear API Key' })).toBeInTheDocument()
  })

  it('shows warning when credentials exist but none is selected', () => {
    vi.mocked(useCredentials).mockReturnValue({ data: [LINEAR_CREDENTIAL] } as ReturnType<typeof useCredentials>)
    renderForm({ ...BASE_CONFIG, credentialId: undefined })
    expect(screen.getByText(/No credential selected/i)).toBeInTheDocument()
  })

  it('changing system calls onChange with new system and credentialId cleared', () => {
    vi.mocked(useCredentials).mockReturnValue({ data: [LINEAR_CREDENTIAL] } as ReturnType<typeof useCredentials>)
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
})
