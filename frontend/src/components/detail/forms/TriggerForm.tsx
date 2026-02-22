import { Clock } from 'lucide-react'
import type { TriggerConfig, TriggerType } from '../../../types'

interface Props {
  config: TriggerConfig
  onChange: (config: TriggerConfig) => void
}

const TRIGGER_TYPES: { value: TriggerType; label: string; available: boolean }[] = [
  { value: 'manual', label: 'Manual', available: true },
  { value: 'cron', label: 'Cron schedule', available: true },
  { value: 'webhook', label: 'Webhook (deferred)', available: false },
  { value: 'connectorevent', label: 'Connector event (deferred)', available: false },
]

export function TriggerForm({ config, onChange }: Props) {
  const update = (patch: Partial<TriggerConfig>) => onChange({ ...config, ...patch })

  return (
    <div className="space-y-4">
      <SectionLabel>Trigger type</SectionLabel>

      <div className="space-y-1">
        {TRIGGER_TYPES.map((tt) => (
          <label
            key={tt.value}
            className={[
              'flex cursor-pointer items-center gap-2',
              !tt.available && 'cursor-not-allowed opacity-40',
            ].join(' ')}
          >
            <input
              type="radio"
              name="trigger-type"
              value={tt.value}
              checked={config.type === tt.value}
              disabled={!tt.available}
              onChange={() => update({ type: tt.value, schedule: undefined })}
              className="accent-blue-500"
            />
            <span className="text-[11px] text-gray-300">{tt.label}</span>
          </label>
        ))}
      </div>

      {/* Manual trigger — no additional config */}
      {config.type === 'manual' && (
        <div className="rounded border border-gray-800 bg-gray-900/40 p-3 text-[11px] text-gray-500">
          This trigger fires when you click <span className="font-medium text-gray-400">Run</span> in the toolbar.
          No additional configuration required.
        </div>
      )}

      {/* Cron trigger — will be expanded by EVA-36 with a visual schedule builder */}
      {config.type === 'cron' && (
        <div className="space-y-3">
          <div>
            <FieldLabel>Cron expression</FieldLabel>
            <input
              type="text"
              value={config.schedule ?? ''}
              placeholder="0 9 * * 1  (Mon 9:00 AM)"
              onChange={(e) => update({ schedule: e.target.value || undefined })}
              className={inputClass + ' font-mono'}
            />
            <p className="mt-1 text-[10px] text-gray-600">
              Standard cron format: minute hour day month weekday
            </p>
          </div>
          {config.schedule && (
            <div className="flex items-center gap-1.5 text-[11px] text-gray-400">
              <Clock size={11} className="text-gray-600" />
              <span className="font-mono text-gray-500">{config.schedule}</span>
            </div>
          )}
          <div className="rounded border border-amber-900/40 bg-amber-950/20 px-2 py-1.5 text-[10px] text-amber-600">
            Visual schedule builder coming in EVA-36
          </div>
        </div>
      )}
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
  return <label className="mb-1 block text-[11px] text-gray-400">{children}</label>
}

const inputClass =
  'w-full rounded border border-gray-700 bg-gray-800 px-2 py-1 text-[11px] text-gray-200 outline-none focus:border-blue-600 focus:ring-0 placeholder:text-gray-600'
