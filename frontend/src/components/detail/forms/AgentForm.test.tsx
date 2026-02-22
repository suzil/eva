import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, vi } from 'vitest'
import { AgentForm } from './AgentForm'
import type { AgentConfig } from '../../../types'

// ---------------------------------------------------------------------------
// Mock Monaco Editor — jsdom has no canvas/worker APIs
// ---------------------------------------------------------------------------

vi.mock('@monaco-editor/react', () => ({
  default: ({
    value,
    onChange,
  }: {
    value?: string
    onChange?: (v: string) => void
  }) => (
    <textarea
      data-testid="monaco-editor"
      value={value ?? ''}
      onChange={(e) => onChange?.(e.target.value)}
    />
  ),
}))

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const BASE_CONFIG: AgentConfig = {
  provider: 'openai',
  model: 'gpt-4o',
  systemPrompt: 'You are helpful.',
  temperature: 0.7,
  maxIterations: 5,
  responseFormat: 'text',
  maxTokens: undefined,
  costBudgetUsd: undefined,
}

function renderForm(config: AgentConfig = BASE_CONFIG, onChange = vi.fn()) {
  return { onChange, ...render(<AgentForm config={config} onChange={onChange} />) }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('AgentForm', () => {
  it('renders OpenAI as the default provider and shows OpenAI model options', () => {
    renderForm()
    const providerSelect = screen.getByDisplayValue('OpenAI')
    expect(providerSelect).toBeInTheDocument()
    expect(screen.getByDisplayValue('gpt-4o')).toBeInTheDocument()
  })

  it('switching provider to Anthropic calls onChange with first Anthropic model', () => {
    const { onChange } = renderForm()
    const providerSelect = screen.getByDisplayValue('OpenAI')
    fireEvent.change(providerSelect, { target: { value: 'anthropic' } })
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ provider: 'anthropic', model: 'claude-opus-4-5' }),
    )
  })

  it('changing model calls onChange with new model value', () => {
    const { onChange } = renderForm()
    const modelSelect = screen.getByDisplayValue('gpt-4o')
    fireEvent.change(modelSelect, { target: { value: 'gpt-4o-mini' } })
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ model: 'gpt-4o-mini' }),
    )
  })

  it('selecting json response format calls onChange with responseFormat: json', () => {
    const { onChange } = renderForm()
    const jsonRadio = screen.getByDisplayValue('json')
    fireEvent.click(jsonRadio)
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ responseFormat: 'json' }),
    )
  })

  it('temperature slider change calls onChange with parsed float', () => {
    const { onChange } = renderForm()
    const slider = screen.getByRole('slider')
    fireEvent.change(slider, { target: { value: '1.2' } })
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ temperature: 1.2 }),
    )
  })

  it('max tokens blur with numeric value calls onChange with integer', () => {
    const { onChange } = renderForm()
    const input = screen.getByPlaceholderText('default')
    fireEvent.change(input, { target: { value: '2048' } })
    fireEvent.blur(input)
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ maxTokens: 2048 }),
    )
  })

  it('max tokens blur with empty string calls onChange with maxTokens: undefined', () => {
    const config = { ...BASE_CONFIG, maxTokens: 512 }
    const { onChange } = renderForm(config)
    const input = screen.getByPlaceholderText('default')
    fireEvent.change(input, { target: { value: '' } })
    fireEvent.blur(input)
    expect(onChange).toHaveBeenCalledWith(
      expect.objectContaining({ maxTokens: undefined }),
    )
  })
})
