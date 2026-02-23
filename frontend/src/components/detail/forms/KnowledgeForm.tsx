import { AlertTriangle } from 'lucide-react'
import Editor from '@monaco-editor/react'
import type { KnowledgeConfig, KnowledgeFormat, RefreshPolicy } from '../../../types'

interface Props {
  config: KnowledgeConfig
  onChange: (config: KnowledgeConfig) => void
}

const FORMATS: { value: KnowledgeFormat; label: string }[] = [
  { value: 'text', label: 'Plain text' },
  { value: 'json', label: 'JSON' },
  { value: 'embedded', label: 'Embedded (vector)' },
]

const REFRESH_POLICIES: { value: RefreshPolicy['type']; label: string }[] = [
  { value: 'static', label: 'Static — load once' },
  { value: 'on_run', label: 'On run — refresh each run' },
  { value: 'periodic', label: 'Periodic — on a schedule' },
]

export function KnowledgeForm({ config, onChange }: Props) {
  const update = (patch: Partial<KnowledgeConfig>) => onChange({ ...config, ...patch })

  const isInline = config.source.type === '_inline_text'
  const inlineValue = config.source.type === '_inline_text' ? config.source.value : ''

  return (
    <div className="space-y-4">
      <SectionLabel>Source</SectionLabel>

      {/* Source tabs — Inline only for MLP; File/URL deferred */}
      <div className="flex gap-1 rounded border border-terminal-500 bg-terminal-900 p-0.5">
        {[
          { id: '_inline_text', label: 'Inline' },
          { id: '_file_ref', label: 'File' },
          { id: '_url_ref', label: 'URL' },
        ].map((tab) => (
          <button
            key={tab.id}
            disabled={tab.id !== '_inline_text'}
            onClick={() => {
              if (tab.id === '_inline_text') {
                update({ source: { type: '_inline_text', value: inlineValue } })
              }
            }}
            className={[
              'flex-1 rounded px-2 py-1 text-[11px] font-medium transition-colors duration-[150ms]',
              isInline && tab.id === '_inline_text'
                ? 'bg-terminal-600 text-terminal-50'
                : 'text-terminal-400 hover:text-terminal-100 disabled:cursor-not-allowed disabled:opacity-40',
            ].join(' ')}
          >
            {tab.label}
          </button>
        ))}
      </div>

      {/* Inline content editor */}
      {isInline && (
        <div>
          <FieldLabel>Content</FieldLabel>
          <div className="overflow-hidden rounded border border-terminal-500 bg-terminal-900">
            <Editor
              height="180px"
              language="markdown"
              theme="eva-dark"
              value={inlineValue}
              onChange={(val) =>
                update({ source: { type: '_inline_text', value: val ?? '' } })
              }
              options={{
                minimap: { enabled: false },
                lineNumbers: 'off',
                fontSize: 12,
                wordWrap: 'on',
                scrollBeyondLastLine: false,
                renderLineHighlight: 'none',
                overviewRulerBorder: false,
                padding: { top: 8, bottom: 8 },
                scrollbar: { vertical: 'auto', horizontal: 'hidden' },
                folding: false,
                lineDecorationsWidth: 0,
                lineNumbersMinChars: 0,
              }}
            />
          </div>
          {!inlineValue?.trim() && (
            <AtFieldWarning message="Content required" />
          )}
        </div>
      )}

      <SectionLabel>Format</SectionLabel>

      <div className="space-y-1">
        {FORMATS.map((f) => (
          <label key={f.value} className="flex cursor-pointer items-center gap-2">
            <input
              type="radio"
              name="knowledge-format"
              value={f.value}
              checked={config.format === f.value}
              onChange={() => update({ format: f.value })}
              className="accent-at-field-500"
            />
            <span className="text-[11px] text-terminal-200">{f.label}</span>
          </label>
        ))}
      </div>

      <SectionLabel>Refresh Policy</SectionLabel>

      <div className="space-y-1">
        {REFRESH_POLICIES.map((rp) => (
          <label key={rp.value} className="flex cursor-pointer items-center gap-2">
            <input
              type="radio"
              name="refresh-policy"
              value={rp.value}
              checked={config.refreshPolicy.type === rp.value}
              onChange={() => {
                const base = { type: rp.value } as RefreshPolicy
                const policy =
                  rp.value === 'periodic'
                    ? { type: 'periodic' as const, periodSeconds: 3600 }
                    : base
                update({ refreshPolicy: policy })
              }}
              className="accent-at-field-500"
            />
            <span className="text-[11px] text-terminal-200">{rp.label}</span>
          </label>
        ))}
      </div>

      {config.refreshPolicy.type === 'periodic' && (
        <div>
          <FieldLabel>Period (seconds)</FieldLabel>
          <input
            type="number"
            min={60}
            value={(config.refreshPolicy as Extract<RefreshPolicy, { type: 'periodic' }>).periodSeconds}
            onChange={(e) => {
              const n = parseInt(e.target.value)
              if (!isNaN(n))
                update({ refreshPolicy: { type: 'periodic', periodSeconds: n } })
            }}
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
