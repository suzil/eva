import { useEffect, useState } from 'react'
import { Check, Loader2 } from 'lucide-react'
import { useLlmSettings, useUpdateLlmSettings } from '../../api/hooks'
import type { LlmProvider } from '../../types'

const OPENAI_MODELS = [
  'gpt-4.1',
  'gpt-4.1-mini',
  'gpt-4.1-nano',
  'gpt-4o',
  'gpt-4o-mini',
  'o3',
  'o4-mini',
]

const ANTHROPIC_MODELS = [
  'claude-opus-4-5',
  'claude-sonnet-4-5',
  'claude-haiku-4-5',
  'claude-3-7-sonnet-20250219',
  'claude-3-5-sonnet-20241022',
  'claude-3-5-haiku-20241022',
]

const DEFAULT_MODELS: Record<LlmProvider, string[]> = {
  openai: OPENAI_MODELS,
  anthropic: ANTHROPIC_MODELS,
}

export function LlmSettingsPanel() {
  const { data: settings, isLoading } = useLlmSettings()
  const updateMut = useUpdateLlmSettings()

  const [provider, setProvider] = useState<LlmProvider>('openai')
  const [model, setModel] = useState('gpt-4o')
  const [apiKey, setApiKey] = useState('')
  const [saved, setSaved] = useState(false)
  const [error, setError] = useState<string | null>(null)

  // Sync form with fetched settings once loaded
  useEffect(() => {
    if (settings) {
      setProvider(settings.provider)
      setModel(settings.model)
    }
  }, [settings])

  const handleProviderChange = (next: LlmProvider) => {
    setProvider(next)
    setModel(DEFAULT_MODELS[next][0])
    setApiKey('')
  }

  const handleSave = async (e: React.FormEvent) => {
    e.preventDefault()
    setError(null)
    setSaved(false)
    try {
      await updateMut.mutateAsync({
        provider,
        model,
        apiKey: apiKey.trim() || undefined,
      })
      setApiKey('')
      setSaved(true)
      setTimeout(() => setSaved(false), 2500)
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to save settings')
    }
  }

  const models = DEFAULT_MODELS[provider]
  const hasStoredKey = settings?.hasKey ?? false

  return (
    <div className="border-b border-terminal-500">
      <div className="flex items-center border-b border-terminal-500 px-3 py-2">
        <span className="font-display text-xs uppercase tracking-widest text-terminal-300">
          MAGI / LLM Provider
        </span>
      </div>

      {isLoading ? (
        <div className="flex items-center justify-center py-6">
          <Loader2 size={16} className="animate-spin text-terminal-400" />
        </div>
      ) : (
        <form onSubmit={handleSave} className="space-y-3 px-3 py-3">
          {/* Provider */}
          <div>
            <FieldLabel>Provider</FieldLabel>
            <div className="flex gap-2">
              {(['openai', 'anthropic'] as LlmProvider[]).map((p) => (
                <button
                  key={p}
                  type="button"
                  onClick={() => handleProviderChange(p)}
                  className={[
                    'flex-1 rounded border px-2 py-1.5 text-[11px] font-medium transition-colors',
                    provider === p
                      ? 'border-at-field-500 bg-at-field-500/10 text-at-field-400'
                      : 'border-terminal-500 text-terminal-400 hover:border-terminal-400 hover:text-terminal-200',
                  ].join(' ')}
                >
                  {p === 'openai' ? 'OpenAI' : 'Anthropic'}
                </button>
              ))}
            </div>
          </div>

          {/* Model */}
          <div>
            <FieldLabel>MAGI Model</FieldLabel>
            <select
              value={model}
              onChange={(e) => setModel(e.target.value)}
              className={selectClass}
            >
              {models.map((m) => (
                <option key={m} value={m}>{m}</option>
              ))}
            </select>
          </div>

          {/* API Key */}
          <div>
            <FieldLabel>
              API Key
              {hasStoredKey && (
                <span className="ml-2 inline-flex items-center gap-1 text-[10px] text-eva-green-400">
                  <Check size={9} />
                  key saved
                </span>
              )}
            </FieldLabel>
            <input
              type="password"
              value={apiKey}
              onChange={(e) => setApiKey(e.target.value)}
              placeholder={hasStoredKey ? 'Enter new key to replace' : 'Paste your API key'}
              className={inputClass}
              autoComplete="off"
            />
          </div>

          {error && (
            <p className="text-[10px] text-nerv-red-400">{error}</p>
          )}

          <button
            type="submit"
            disabled={updateMut.isPending}
            className="flex items-center gap-1.5 rounded bg-at-field-500 px-2.5 py-1.5 text-[11px] font-medium text-terminal-950 hover:bg-at-field-600 disabled:opacity-50 transition-colors"
          >
            {updateMut.isPending
              ? <Loader2 size={11} className="animate-spin" />
              : saved
              ? <Check size={11} />
              : null}
            {saved ? 'Saved' : 'Save'}
          </button>
        </form>
      )}
    </div>
  )
}

function FieldLabel({ children }: { children: React.ReactNode }) {
  return <label className="mb-1 flex items-center text-[11px] text-terminal-300">{children}</label>
}

const inputClass =
  'w-full rounded border border-terminal-500 bg-terminal-700 px-2 py-1 text-[11px] text-terminal-100 outline-none focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 placeholder:text-terminal-400 transition-colors'

const selectClass =
  'w-full rounded border border-terminal-500 bg-terminal-700 px-2 py-1 text-[11px] text-terminal-100 outline-none focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors'
