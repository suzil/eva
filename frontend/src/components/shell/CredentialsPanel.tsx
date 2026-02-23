import { useState } from 'react'
import { KeyRound, Plus, Trash2, Loader2 } from 'lucide-react'
import { useCredentials, useCreateCredential, useDeleteCredential } from '../../api/hooks'
import type { SystemType, CredentialType, CreateCredentialReq } from '../../types'

const SYSTEMS: { value: SystemType; label: string }[] = [
  { value: 'linear', label: 'Linear' },
  { value: 'github', label: 'GitHub' },
  { value: 'http', label: 'HTTP / REST' },
  { value: 'codebase', label: 'Codebase' },
]

const CRED_TYPES: { value: CredentialType; label: string }[] = [
  { value: 'api_key', label: 'API Key' },
  { value: 'oauth_token', label: 'OAuth Token' },
]

const SYSTEM_LABELS: Record<SystemType, string> = {
  linear: 'Linear',
  github: 'GitHub',
  http: 'HTTP / REST',
  codebase: 'Codebase',
}

const TYPE_LABELS: Record<CredentialType, string> = {
  api_key: 'API Key',
  oauth_token: 'OAuth Token',
}

const EMPTY_FORM: CreateCredentialReq = {
  name: '',
  system: 'linear',
  type: 'api_key',
  secret: '',
}

export function CredentialsPanel() {
  const { data: credentials = [], isLoading } = useCredentials()
  const createMut = useCreateCredential()
  const deleteMut = useDeleteCredential()

  const [showForm, setShowForm] = useState(false)
  const [form, setForm] = useState<CreateCredentialReq>(EMPTY_FORM)
  const [formError, setFormError] = useState<string | null>(null)
  const [deletingId, setDeletingId] = useState<string | null>(null)

  const update = (patch: Partial<CreateCredentialReq>) => setForm((f) => ({ ...f, ...patch }))

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    setFormError(null)
    if (!form.name.trim()) { setFormError('Name is required'); return }
    if (!form.secret.trim()) { setFormError('Secret value is required'); return }
    try {
      await createMut.mutateAsync(form)
      setForm(EMPTY_FORM)
      setShowForm(false)
    } catch (err) {
      setFormError(err instanceof Error ? err.message : 'Failed to save credential')
    }
  }

  const handleDelete = async (id: string) => {
    setDeletingId(id)
    try {
      await deleteMut.mutateAsync(id)
    } finally {
      setDeletingId(null)
    }
  }

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Header row */}
      <div className="flex items-center justify-between border-b border-terminal-500 px-3 py-2">
        <span className="font-display text-xs uppercase tracking-widest text-terminal-300">
          Credentials
        </span>
        <button
          onClick={() => { setShowForm((v) => !v); setFormError(null) }}
          className="flex items-center gap-1 rounded px-1.5 py-1 text-[11px] text-terminal-400 hover:bg-terminal-600 hover:text-terminal-100 transition-colors"
          title="Add credential"
        >
          <Plus size={12} />
          Add
        </button>
      </div>

      {/* Add form */}
      {showForm && (
        <form onSubmit={handleSubmit} className="border-b border-terminal-500 bg-terminal-900/60 px-3 py-3 space-y-2">
          <p className="font-display text-xs uppercase tracking-widest text-terminal-300 mb-2">
            New Credential
          </p>

          <div>
            <FieldLabel>Name</FieldLabel>
            <input
              type="text"
              value={form.name}
              onChange={(e) => update({ name: e.target.value })}
              placeholder="e.g. Linear workspace key"
              className={inputClass}
              autoFocus
            />
          </div>

          <div>
            <FieldLabel>System</FieldLabel>
            <select
              value={form.system}
              onChange={(e) => update({ system: e.target.value as SystemType })}
              className={selectClass}
            >
              {SYSTEMS.map((s) => (
                <option key={s.value} value={s.value}>{s.label}</option>
              ))}
            </select>
          </div>

          <div>
            <FieldLabel>Type</FieldLabel>
            <select
              value={form.type}
              onChange={(e) => update({ type: e.target.value as CredentialType })}
              className={selectClass}
            >
              {CRED_TYPES.map((t) => (
                <option key={t.value} value={t.value}>{t.label}</option>
              ))}
            </select>
          </div>

          <div>
            <FieldLabel>Secret value</FieldLabel>
            <input
              type="password"
              value={form.secret}
              onChange={(e) => update({ secret: e.target.value })}
              placeholder="Paste your API key or token"
              className={inputClass}
            />
          </div>

          {formError && (
            <p className="text-[10px] text-nerv-red-400">{formError}</p>
          )}

          <div className="flex gap-2 pt-1">
            <button
              type="submit"
              disabled={createMut.isPending}
              className="flex items-center gap-1.5 rounded bg-at-field-500 px-2.5 py-1.5 text-[11px] font-medium text-terminal-950 hover:bg-at-field-600 disabled:opacity-50 transition-colors"
            >
              {createMut.isPending ? <Loader2 size={11} className="animate-spin" /> : null}
              Save
            </button>
            <button
              type="button"
              onClick={() => { setShowForm(false); setForm(EMPTY_FORM); setFormError(null) }}
              className="rounded px-2.5 py-1.5 text-[11px] text-terminal-400 hover:bg-terminal-600 hover:text-terminal-100 transition-colors"
            >
              Cancel
            </button>
          </div>
        </form>
      )}

      {/* Credential list */}
      <div className="flex-1 overflow-y-auto">
        {isLoading ? (
          <div className="flex items-center justify-center py-8">
            <Loader2 size={16} className="animate-spin text-terminal-400" />
          </div>
        ) : credentials.length === 0 ? (
          <div className="flex flex-col items-center justify-center gap-2 py-10 px-4">
            <KeyRound size={24} className="text-terminal-600" />
            <p className="text-[11px] text-terminal-400 text-center">
              No credentials yet.<br />Add an API key to connect integrations.
            </p>
          </div>
        ) : (
          <ul className="divide-y divide-terminal-600/60">
            {credentials.map((cred) => (
              <li key={cred.id} className="flex items-center gap-2 px-3 py-2.5 group hover:bg-terminal-700/30">
                <KeyRound size={11} className="shrink-0 text-terminal-400" />
                <div className="flex flex-1 flex-col min-w-0">
                  <span className="truncate text-[11px] text-terminal-100">{cred.name}</span>
                  <span className="text-[10px] text-terminal-400">
                    {SYSTEM_LABELS[cred.system]} · {TYPE_LABELS[cred.type]}
                  </span>
                </div>
                <button
                  onClick={() => handleDelete(cred.id)}
                  disabled={deletingId === cred.id}
                  className="shrink-0 rounded p-1 text-terminal-500 opacity-0 group-hover:opacity-100 hover:bg-nerv-red-900/40 hover:text-nerv-red-400 disabled:opacity-50 transition-all"
                  title="Delete credential"
                  aria-label={`Delete ${cred.name}`}
                >
                  {deletingId === cred.id
                    ? <Loader2 size={11} className="animate-spin" />
                    : <Trash2 size={11} />
                  }
                </button>
              </li>
            ))}
          </ul>
        )}
      </div>
    </div>
  )
}

function FieldLabel({ children }: { children: React.ReactNode }) {
  return <label className="mb-1 block text-[11px] text-terminal-300">{children}</label>
}

const inputClass =
  'w-full rounded border border-terminal-500 bg-terminal-700 px-2 py-1 text-[11px] text-terminal-100 outline-none focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 placeholder:text-terminal-400 transition-colors'

const selectClass =
  'w-full rounded border border-terminal-500 bg-terminal-700 px-2 py-1 text-[11px] text-terminal-100 outline-none focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors'
