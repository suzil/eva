import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, vi } from 'vitest'
import { TriggerForm } from './TriggerForm'
import type { TriggerConfig } from '../../../types'

const baseConfig: TriggerConfig = { type: 'cron', schedule: undefined }

function renderForm(config: TriggerConfig, onChange = vi.fn()) {
  return { onChange, ...render(<TriggerForm config={config} onChange={onChange} />) }
}

describe('TriggerForm — cron section', () => {
  it('renders preset chips when trigger type is cron', () => {
    renderForm(baseConfig)
    expect(screen.getByRole('button', { name: 'Every hour' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Daily 9 AM' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Mon 9:00' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Every 15 min' })).toBeInTheDocument()
  })

  it('clicking a preset calls onChange with that schedule', () => {
    const { onChange } = renderForm(baseConfig)
    fireEvent.click(screen.getByRole('button', { name: 'Every hour' }))
    expect(onChange).toHaveBeenCalledWith(expect.objectContaining({ schedule: '0 * * * *' }))
  })

  it('shows human-readable preview for a valid expression', () => {
    renderForm({ type: 'cron', schedule: '0 9 * * 1' })
    // cronstrue converts "0 9 * * 1" to an English description containing "9:00 AM"
    const matches = screen.getAllByText(/9:00 AM/i)
    expect(matches.length).toBeGreaterThan(0)
  })

  it('shows next fire time for a valid expression', () => {
    renderForm({ type: 'cron', schedule: '0 9 * * 1' })
    expect(screen.getByText(/Next:/i)).toBeInTheDocument()
  })

  it('shows inline error for an invalid cron expression', () => {
    renderForm({ type: 'cron', schedule: 'not-a-cron' })
    // Error message should appear; preview should not
    expect(screen.queryByText(/Next:/i)).not.toBeInTheDocument()
    // Some error text should be visible (content varies by parser version)
    const input = screen.getByPlaceholderText('0 9 * * 1')
    expect(input).toHaveClass('border-red-600')
  })

  it('does not show preview when expression is empty', () => {
    renderForm({ type: 'cron', schedule: undefined })
    expect(screen.queryByText(/Next:/i)).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: /^Clock/ })).not.toBeInTheDocument()
  })

  it('typing in the input calls onChange with the new schedule', () => {
    const { onChange } = renderForm(baseConfig)
    const input = screen.getByPlaceholderText('0 9 * * 1')
    fireEvent.change(input, { target: { value: '0 * * * *' } })
    expect(onChange).toHaveBeenCalledWith(expect.objectContaining({ schedule: '0 * * * *' }))
  })

  it('does not render cron section when trigger type is manual', () => {
    renderForm({ type: 'manual' })
    expect(screen.queryByPlaceholderText('0 9 * * 1')).not.toBeInTheDocument()
    expect(screen.getByText(/This trigger fires/i)).toBeInTheDocument()
  })
})
