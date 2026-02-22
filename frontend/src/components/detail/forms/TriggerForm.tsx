import { useMemo } from 'react'
import { Clock } from 'lucide-react'
import cronstrue from 'cronstrue'
import { CronExpressionParser } from 'cron-parser'
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

const PRESETS: { label: string; value: string }[] = [
  { label: 'Every hour', value: '0 * * * *' },
  { label: 'Daily 9 AM', value: '0 9 * * *' },
  { label: 'Mon 9:00', value: '0 9 * * 1' },
  { label: 'Every 15 min', value: '*/15 * * * *' },
]

function parseCron(schedule: string): { description: string; nextFire: Date } | { error: string } {
  if (!schedule.trim()) return { error: '' }
  try {
    const description = cronstrue.toString(schedule, {
      verbose: false,
      throwExceptionOnParseError: true,
    })
    const expr = CronExpressionParser.parse(schedule)
    const nextFire = expr.next().toDate()
    return { description, nextFire }
  } catch (e) {
    return { error: e instanceof Error ? e.message.replace(/^.*?:\s*/, '') : 'Invalid expression' }
  }
}

function formatNextFire(date: Date): string {
  return new Intl.DateTimeFormat(undefined, {
    weekday: 'short',
    month: 'short',
    day: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
  }).format(date)
}

export function TriggerForm({ config, onChange }: Props) {
  const update = (patch: Partial<TriggerConfig>) => onChange({ ...config, ...patch })

  const cronResult = useMemo(
    () => (config.type === 'cron' ? parseCron(config.schedule ?? '') : null),
    [config.type, config.schedule],
  )

  const validationError =
    cronResult && 'error' in cronResult && cronResult.error ? cronResult.error : null
  const cronDescription = cronResult && 'description' in cronResult ? cronResult.description : null
  const cronNextFire = cronResult && 'nextFire' in cronResult ? cronResult.nextFire : null

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
          This trigger fires when you click <span className="font-medium text-gray-400">Run</span>{' '}
          in the toolbar. No additional configuration required.
        </div>
      )}

      {/* Cron trigger */}
      {config.type === 'cron' && (
        <div className="space-y-3">
          {/* Presets */}
          <div>
            <FieldLabel>Presets</FieldLabel>
            <div className="flex flex-wrap gap-1">
              {PRESETS.map((p) => (
                <button
                  key={p.value}
                  type="button"
                  onClick={() => update({ schedule: p.value })}
                  className={[
                    'rounded border px-2 py-0.5 text-[10px] transition-colors',
                    config.schedule === p.value
                      ? 'border-blue-600 bg-blue-600/20 text-blue-300'
                      : 'border-gray-700 bg-gray-800 text-gray-400 hover:border-gray-600 hover:text-gray-300',
                  ].join(' ')}
                >
                  {p.label}
                </button>
              ))}
            </div>
          </div>

          {/* Expression input */}
          <div>
            <FieldLabel>Cron expression</FieldLabel>
            <input
              type="text"
              value={config.schedule ?? ''}
              placeholder="0 9 * * 1"
              onChange={(e) => update({ schedule: e.target.value || undefined })}
              className={[
                inputClass,
                'font-mono',
                validationError ? 'border-red-600 focus:border-red-500' : '',
              ]
                .filter(Boolean)
                .join(' ')}
            />
            <p className="mt-1 text-[10px] text-gray-600">
              minute &nbsp; hour &nbsp; day-of-month &nbsp; month &nbsp; day-of-week
            </p>
            {validationError && (
              <p className="mt-1 text-[10px] text-red-500">{validationError}</p>
            )}
          </div>

          {/* Preview card — only shown for valid, non-empty expressions */}
          {cronDescription && cronNextFire && (
            <div className="space-y-1.5 rounded border border-gray-800 bg-gray-900/40 px-2.5 py-2">
              <div className="flex items-center gap-1.5">
                <Clock size={11} className="shrink-0 text-blue-500" />
                <span className="text-[11px] text-gray-200">{cronDescription}</span>
              </div>
              <div className="flex items-center gap-1.5 pl-0.5">
                <span className="text-[10px] text-gray-600">Next:</span>
                <span className="text-[10px] text-gray-500">{formatNextFire(cronNextFire)}</span>
              </div>
            </div>
          )}
        </div>
      )}
    </div>
  )
}

function SectionLabel({ children }: { children: React.ReactNode }) {
  return (
    <p className="text-[10px] font-semibold uppercase tracking-wider text-gray-600">{children}</p>
  )
}

function FieldLabel({ children }: { children: React.ReactNode }) {
  return <label className="mb-1 block text-[11px] text-gray-400">{children}</label>
}

const inputClass =
  'w-full rounded border border-gray-700 bg-gray-800 px-2 py-1 text-[11px] text-gray-200 outline-none focus:border-blue-600 focus:ring-0 placeholder:text-gray-600'
