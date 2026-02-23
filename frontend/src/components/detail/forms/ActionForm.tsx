import { AlertTriangle } from 'lucide-react'
import type { ActionConfig, ActionOperation, ErrorHandlingMode } from '../../../types'

interface Props {
  config: ActionConfig
  onChange: (config: ActionConfig) => void
}

const OPERATIONS: { value: ActionOperation; label: string; available: boolean }[] = [
  { value: 'template', label: 'Template', available: true },
  { value: 'code', label: 'Code (deferred)', available: false },
  { value: 'api_call', label: 'API call (deferred)', available: false },
  { value: 'format', label: 'Format', available: true },
]

const ERROR_MODES: { value: ErrorHandlingMode['mode']; label: string }[] = [
  { value: 'fail', label: 'Fail — stop on error' },
  { value: 'continue', label: 'Continue — pass through' },
  { value: 'use_default', label: 'Use default value' },
]

export function ActionForm({ config, onChange }: Props) {
  const update = (patch: Partial<ActionConfig>) => onChange({ ...config, ...patch })

  const templateValue =
    config.operation === 'template' && typeof config.parameters === 'object'
      ? String((config.parameters as Record<string, unknown>).template ?? '')
      : ''

  return (
    <div className="space-y-4">
      <SectionLabel>Operation</SectionLabel>

      {/* Operation type */}
      <div>
        <FieldLabel>Type</FieldLabel>
        <select
          value={config.operation}
          onChange={(e) =>
            update({ operation: e.target.value as ActionOperation, parameters: {} })
          }
          className={selectClass}
        >
          {OPERATIONS.map((op) => (
            <option key={op.value} value={op.value} disabled={!op.available}>
              {op.label}
            </option>
          ))}
        </select>
      </div>

      {/* Template operation parameter */}
      {config.operation === 'template' && (
        <div>
          <FieldLabel>Template</FieldLabel>
          <textarea
            value={templateValue}
            rows={5}
            placeholder="Use {{input}} to reference upstream output."
            onChange={(e) =>
              update({ parameters: { template: e.target.value } })
            }
            className={[
              'w-full resize-y rounded border border-terminal-500 bg-terminal-700 px-2 py-1.5',
              'font-mono text-sm text-terminal-100 outline-none',
              'placeholder:text-terminal-400 focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30',
              'transition-colors duration-[150ms]',
            ].join(' ')}
          />
          {!templateValue.trim() && (
            <AtFieldWarning message="Template required" />
          )}
          <p className="mt-1 font-mono text-[10px] text-terminal-400">
            Use {'{{portName}}'} or {'{{portName.field}}'} for substitution.
          </p>
        </div>
      )}

      <SectionLabel>Error Handling</SectionLabel>

      <div className="space-y-1">
        {ERROR_MODES.map((em) => (
          <label key={em.value} className="flex cursor-pointer items-center gap-2">
            <input
              type="radio"
              name="error-handling"
              value={em.value}
              checked={config.errorHandling.mode === em.value}
              onChange={() => {
                const mode: ErrorHandlingMode =
                  em.value === 'use_default'
                    ? { mode: 'use_default', value: '' }
                    : { mode: em.value }
                update({ errorHandling: mode })
              }}
              className="accent-at-field-500"
            />
            <span className="text-[11px] text-terminal-200">{em.label}</span>
          </label>
        ))}
      </div>

      {config.errorHandling.mode === 'use_default' && (
        <div>
          <FieldLabel>Default value</FieldLabel>
          <input
            type="text"
            value={(config.errorHandling as Extract<ErrorHandlingMode, { mode: 'use_default' }>).value}
            placeholder="Fallback output"
            onChange={(e) =>
              update({ errorHandling: { mode: 'use_default', value: e.target.value } })
            }
            className={inputClass}
          />
        </div>
      )}
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
  return <label className="mb-1 block text-sm font-medium text-terminal-200">{children}</label>
}

const inputClass =
  'w-full rounded border border-terminal-500 bg-terminal-700 px-2 py-1 text-sm text-terminal-100 outline-none placeholder:text-terminal-400 focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors duration-[150ms]'

const selectClass =
  'w-full rounded border border-terminal-500 bg-terminal-700 px-2 py-1 text-sm text-terminal-100 outline-none focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors duration-[150ms]'
