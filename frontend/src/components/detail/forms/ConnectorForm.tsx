import { AlertTriangle, Settings } from 'lucide-react'
import type { ConnectorConfig, SystemType } from '../../../types'
import { useCredentials } from '../../../api/hooks'

interface Props {
  config: ConnectorConfig
  onChange: (config: ConnectorConfig) => void
}

const SYSTEMS: { value: SystemType; label: string }[] = [
  { value: 'linear', label: 'Linear' },
  { value: 'github', label: 'GitHub' },
  { value: 'http', label: 'HTTP / REST' },
  { value: 'codebase', label: 'Codebase' },
]

export function ConnectorForm({ config, onChange }: Props) {
  const update = (patch: Partial<ConnectorConfig>) => onChange({ ...config, ...patch })
  const { data: allCredentials = [] } = useCredentials()
  const systemCredentials = allCredentials.filter((c) => c.system === config.system)

  return (
    <div className="space-y-4">
      <SectionLabel>Integration</SectionLabel>

      {/* System type */}
      <div>
        <FieldLabel>System</FieldLabel>
        <select
          value={config.system}
          onChange={(e) => {
            update({ system: e.target.value as SystemType, credentialId: undefined })
          }}
          className={selectClass}
        >
          {SYSTEMS.map((s) => (
            <option key={s.value} value={s.value}>{s.label}</option>
          ))}
        </select>
      </div>

      {/* Credential picker */}
      <div>
        <FieldLabel>Credential</FieldLabel>
        {systemCredentials.length === 0 ? (
          <div className="flex items-center gap-2 rounded border border-terminal-500 bg-terminal-800/50 px-2 py-2">
            <Settings size={11} className="shrink-0 text-terminal-400" />
            <span className="text-[11px] text-terminal-400">
              No credentials for this system.{' '}
              <span className="font-medium text-terminal-200">Add one in Settings → Credentials.</span>
            </span>
          </div>
        ) : (
          <>
            <select
              value={config.credentialId ?? ''}
              onChange={(e) => update({ credentialId: e.target.value || undefined })}
              className={selectClass}
            >
              <option value="">— select credential —</option>
              {systemCredentials.map((c) => (
                <option key={c.id} value={c.id}>{c.name}</option>
              ))}
            </select>
            {!config.credentialId && (
              <AtFieldWarning message="No credential selected — connector will fail at runtime." />
            )}
          </>
        )}
      </div>

      <SectionLabel>Scope</SectionLabel>

      {/* Endpoint */}
      <div>
        <FieldLabel>Endpoint (optional)</FieldLabel>
        <input
          type="text"
          value={config.endpoint ?? ''}
          placeholder="https://…"
          onChange={(e) => update({ endpoint: e.target.value || undefined })}
          className={inputClass}
        />
      </div>

      {/* Scope */}
      <div>
        <FieldLabel>Scope (optional)</FieldLabel>
        <input
          type="text"
          value={config.scope ?? ''}
          placeholder="e.g. read:issues"
          onChange={(e) => update({ scope: e.target.value || undefined })}
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
  return <label className="mb-1 block text-sm font-medium text-terminal-200">{children}</label>
}

const inputClass =
  'w-full rounded border border-terminal-500 bg-terminal-700 px-2 py-1 text-sm text-terminal-100 outline-none placeholder:text-terminal-400 focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors duration-[150ms]'

const selectClass =
  'w-full rounded border border-terminal-500 bg-terminal-700 px-2 py-1 text-sm text-terminal-100 outline-none focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors duration-[150ms]'
