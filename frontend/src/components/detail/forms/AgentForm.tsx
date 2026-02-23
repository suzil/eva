import { useState } from 'react'
import { AlertTriangle } from 'lucide-react'
import Editor from '@monaco-editor/react'
import type { AgentConfig, LLMProvider, ResponseFormat } from '../../../types'

const OPENAI_MODELS = [
  'gpt-4o',
  'gpt-4o-mini',
  'gpt-4-turbo',
  'gpt-3.5-turbo',
]

const ANTHROPIC_MODELS = [
  'claude-opus-4-5',
  'claude-sonnet-4-5',
  'claude-3-5-sonnet-20241022',
  'claude-3-5-haiku-20241022',
]

const PROVIDER_LABELS: Record<LLMProvider, string> = {
  openai: 'OpenAI',
  anthropic: 'Anthropic',
}

interface Props {
  config: AgentConfig
  onChange: (config: AgentConfig) => void
}

export function AgentForm({ config, onChange }: Props) {
  const [tempTokens, setTempTokens] = useState(String(config.maxTokens ?? ''))
  const [tempCost, setTempCost] = useState(String(config.costBudgetUsd ?? ''))

  const update = (patch: Partial<AgentConfig>) => onChange({ ...config, ...patch })

  const activeProvider: LLMProvider = config.provider ?? 'openai'
  const modelList = activeProvider === 'anthropic' ? ANTHROPIC_MODELS : OPENAI_MODELS

  const handleProviderChange = (provider: LLMProvider) => {
    const models = provider === 'anthropic' ? ANTHROPIC_MODELS : OPENAI_MODELS
    update({ provider, model: models[0] })
  }

  return (
    <div className="space-y-4">
      <SectionLabel>Model</SectionLabel>

      {/* Provider picker */}
      <div>
        <FieldLabel>Provider</FieldLabel>
        <select
          value={activeProvider}
          onChange={(e) => handleProviderChange(e.target.value as LLMProvider)}
          className={selectClass}
        >
          {(Object.keys(PROVIDER_LABELS) as LLMProvider[]).map((p) => (
            <option key={p} value={p}>{PROVIDER_LABELS[p]}</option>
          ))}
        </select>
      </div>

      {/* Model picker */}
      <div>
        <FieldLabel>Model</FieldLabel>
        <select
          value={config.model}
          onChange={(e) => update({ model: e.target.value })}
          className={selectClass}
        >
          {modelList.map((m) => (
            <option key={m} value={m}>{m}</option>
          ))}
          {!modelList.includes(config.model) && (
            <option value={config.model}>{config.model}</option>
          )}
        </select>
      </div>

      {/* Response format */}
      <div>
        <FieldLabel>Response format</FieldLabel>
        <div className="flex gap-2">
          {(['text', 'json'] as ResponseFormat[]).map((f) => (
            <label key={f} className="flex cursor-pointer items-center gap-1.5">
              <input
                type="radio"
                name={`response-format-${config.model}`}
                value={f}
                checked={config.responseFormat === f}
                onChange={() => update({ responseFormat: f })}
                className="accent-at-field-500"
              />
              <span className="capitalize text-[11px] text-terminal-200">{f}</span>
            </label>
          ))}
        </div>
      </div>

      <SectionLabel>System Prompt</SectionLabel>

      {/* Monaco system prompt editor */}
      <div>
        <FieldLabel>System prompt</FieldLabel>
        <div className="overflow-hidden rounded border border-terminal-500 bg-terminal-900">
          <Editor
            height="200px"
            language="markdown"
            theme="eva-dark"
            value={config.systemPrompt}
            onChange={(val) => update({ systemPrompt: val ?? '' })}
            options={{
              minimap: { enabled: false },
              lineNumbers: 'off',
              fontSize: 12,
              wordWrap: 'on',
              scrollBeyondLastLine: false,
              renderLineHighlight: 'none',
              overviewRulerBorder: false,
              hideCursorInOverviewRuler: true,
              padding: { top: 8, bottom: 8 },
              scrollbar: { vertical: 'auto', horizontal: 'hidden' },
              folding: false,
              lineDecorationsWidth: 0,
              lineNumbersMinChars: 0,
            }}
          />
        </div>
        {!config.systemPrompt?.trim() && (
          <AtFieldWarning message="System prompt required for agent to run" />
        )}
        <p className="mt-1 font-mono text-[10px] text-terminal-400">
          Use {'{{portName}}'} to reference upstream port values.
        </p>
      </div>

      <SectionLabel>Parameters</SectionLabel>

      {/* Temperature */}
      <div>
        <div className="mb-1 flex items-center justify-between">
          <FieldLabel>Temperature</FieldLabel>
          <span className="font-mono text-[11px] text-terminal-300">
            {config.temperature.toFixed(1)}
          </span>
        </div>
        <input
          type="range"
          min={0}
          max={2}
          step={0.1}
          value={config.temperature}
          onChange={(e) => update({ temperature: parseFloat(e.target.value) })}
          className="h-1.5 w-full cursor-pointer appearance-none rounded bg-terminal-600 accent-at-field-500"
        />
        <div className="mt-0.5 flex justify-between text-[9px] text-terminal-400">
          <span>Precise</span>
          <span>Creative</span>
        </div>
      </div>

      {/* Max tokens */}
      <div>
        <FieldLabel>Max tokens (optional)</FieldLabel>
        <input
          type="number"
          value={tempTokens}
          placeholder="default"
          min={1}
          onChange={(e) => setTempTokens(e.target.value)}
          onBlur={() => {
            const n = parseInt(tempTokens)
            update({ maxTokens: isNaN(n) || tempTokens === '' ? undefined : n })
          }}
          className={inputClass}
        />
      </div>

      {/* Max iterations */}
      <div>
        <FieldLabel>Max iterations</FieldLabel>
        <input
          type="number"
          value={config.maxIterations}
          min={1}
          max={50}
          onChange={(e) => {
            const n = parseInt(e.target.value)
            if (!isNaN(n)) update({ maxIterations: n })
          }}
          className={inputClass}
        />
      </div>

      {/* Cost budget */}
      <div>
        <FieldLabel>Cost budget USD (optional)</FieldLabel>
        <input
          type="number"
          value={tempCost}
          placeholder="unlimited"
          min={0}
          step={0.01}
          onChange={(e) => setTempCost(e.target.value)}
          onBlur={() => {
            const n = parseFloat(tempCost)
            update({ costBudgetUsd: isNaN(n) || tempCost === '' ? undefined : n })
          }}
          className={inputClass}
        />
      </div>
    </div>
  )
}

function AtFieldWarning({ message }: { message: string }) {
  return (
    <div className="mt-1.5 flex items-center gap-1.5 rounded border border-warn-amber-700 bg-warn-amber-950/40 px-2 py-1 text-[10px] text-warn-amber-400">
      <AlertTriangle size={10} className="shrink-0" />
      <span>{message}</span>
    </div>
  )
}

function SectionLabel({ children }: { children: React.ReactNode }) {
  return (
    <p className="font-display text-[10px] uppercase tracking-widest text-terminal-300">
      {children}
    </p>
  )
}

function FieldLabel({ children }: { children: React.ReactNode }) {
  return (
    <label className="mb-1 block text-sm font-medium text-terminal-200">{children}</label>
  )
}

const inputClass =
  'w-full rounded border border-terminal-500 bg-terminal-700 px-2 py-1 text-sm text-terminal-100 outline-none placeholder:text-terminal-400 focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors duration-[150ms]'

const selectClass =
  'w-full rounded border border-terminal-500 bg-terminal-700 px-2 py-1 text-sm text-terminal-100 outline-none focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors duration-[150ms]'
