import { Settings } from 'lucide-react'
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
          <div className="flex items-center gap-2 rounded border border-gray-700 bg-gray-800/50 px-2 py-2">
            <Settings size={11} className="shrink-0 text-gray-600" />
            <span className="text-[11px] text-gray-500">
              No credentials for this system.{' '}
              <span className="font-medium text-gray-400">Add one in Settings → Credentials.</span>
            </span>
          </div>
        ) : (
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

const selectClass =
  'w-full rounded border border-gray-700 bg-gray-800 px-2 py-1 text-[11px] text-gray-200 outline-none focus:border-blue-600'
