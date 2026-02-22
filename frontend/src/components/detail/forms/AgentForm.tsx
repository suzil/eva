import { useState } from 'react'
import Editor from '@monaco-editor/react'
import type { AgentConfig, ResponseFormat } from '../../../types'

const MODELS = [
  'gpt-4o',
  'gpt-4o-mini',
  'gpt-4-turbo',
  'gpt-3.5-turbo',
]

interface Props {
  config: AgentConfig
  onChange: (config: AgentConfig) => void
}

export function AgentForm({ config, onChange }: Props) {
  // Local draft for number inputs so the field is usable while typing
  const [tempTokens, setTempTokens] = useState(String(config.maxTokens ?? ''))
  const [tempCost, setTempCost] = useState(String(config.costBudgetUsd ?? ''))

  const update = (patch: Partial<AgentConfig>) => onChange({ ...config, ...patch })

  return (
    <div className="space-y-4">
      <SectionLabel>Model</SectionLabel>

      {/* Model picker */}
      <div>
        <FieldLabel>Model</FieldLabel>
        <select
          value={config.model}
          onChange={(e) => update({ model: e.target.value })}
          className={selectClass}
        >
          {MODELS.map((m) => (
            <option key={m} value={m}>{m}</option>
          ))}
          {!MODELS.includes(config.model) && (
            <option value={config.model}>{config.model}</option>
          )}
        </select>
      </div>

      {/* Response format */}
      <div>
        <FieldLabel>Response format</FieldLabel>
        <div className="flex gap-2">
          {(['text', 'json'] as ResponseFormat[]).map((f) => (
            <label key={f} className="flex items-center gap-1.5 cursor-pointer">
              <input
                type="radio"
                name={`response-format-${config.model}`}
                value={f}
                checked={config.responseFormat === f}
                onChange={() => update({ responseFormat: f })}
                className="accent-blue-500"
              />
              <span className="text-[11px] text-gray-300 capitalize">{f}</span>
            </label>
          ))}
        </div>
      </div>

      <SectionLabel>System Prompt</SectionLabel>

      {/* Monaco system prompt editor */}
      <div>
        <FieldLabel>System prompt</FieldLabel>
        <div className="overflow-hidden rounded border border-gray-700 bg-[#1e1e1e]">
          <Editor
            height="200px"
            language="markdown"
            theme="vs-dark"
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
        <p className="mt-1 text-[10px] text-gray-600">
          Use {'{{portName}}'} to reference upstream port values.
        </p>
      </div>

      <SectionLabel>Parameters</SectionLabel>

      {/* Temperature */}
      <div>
        <div className="mb-1 flex items-center justify-between">
          <FieldLabel>Temperature</FieldLabel>
          <span className="text-[11px] font-mono text-gray-400">
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
          className="h-1.5 w-full cursor-pointer appearance-none rounded bg-gray-700 accent-blue-500"
        />
        <div className="mt-0.5 flex justify-between text-[9px] text-gray-600">
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

function SectionLabel({ children }: { children: React.ReactNode }) {
  return (
    <p className="text-[10px] font-semibold uppercase tracking-wider text-gray-600">
      {children}
    </p>
  )
}

function FieldLabel({ children }: { children: React.ReactNode }) {
  return (
    <label className="mb-1 block text-[11px] text-gray-400">{children}</label>
  )
}

const inputClass =
  'w-full rounded border border-gray-700 bg-gray-800 px-2 py-1 text-[11px] text-gray-200 outline-none focus:border-blue-600 focus:ring-0 placeholder:text-gray-600'

const selectClass =
  'w-full rounded border border-gray-700 bg-gray-800 px-2 py-1 text-[11px] text-gray-200 outline-none focus:border-blue-600'
