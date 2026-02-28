import { useState } from 'react'
import { AlertTriangle, Search } from 'lucide-react'
import Editor from '@monaco-editor/react'
import type { KnowledgeConfig, KnowledgeFormat, RefreshPolicy } from '../../../types'
import { useKnowledgeSearch } from '../../../api/hooks'

interface Props {
  config: KnowledgeConfig
  onChange: (config: KnowledgeConfig) => void
  programId?: string
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

export function KnowledgeForm({ config, onChange, programId = '' }: Props) {
  const update = (patch: Partial<KnowledgeConfig>) => onChange({ ...config, ...patch })

  const sourceType = config.source.type
  const isInline = sourceType === '_inline_text'
  const isLibrary = sourceType === '_library_ref'
  const inlineValue = sourceType === '_inline_text' ? config.source.value : ''

  return (
    <div className="space-y-4">
      <SectionLabel>Source</SectionLabel>

      {/* Source tabs */}
      <div className="flex gap-1 rounded border border-terminal-500 bg-terminal-900 p-0.5">
        {[
          { id: '_inline_text', label: 'Inline' },
          { id: '_file_ref', label: 'File' },
          { id: '_url_ref', label: 'URL' },
          { id: '_library_ref', label: 'Library' },
        ].map((tab) => {
          const isActive = sourceType === tab.id
          const isEnabled = tab.id === '_inline_text' || tab.id === '_library_ref'
          return (
            <button
              key={tab.id}
              disabled={!isEnabled}
              onClick={() => {
                if (tab.id === '_inline_text') {
                  update({ source: { type: '_inline_text', value: inlineValue } })
                } else if (tab.id === '_library_ref') {
                  update({ source: { type: '_library_ref', value: '' } })
                }
              }}
              className={[
                'flex-1 rounded px-2 py-1 text-[11px] font-medium transition-colors duration-[150ms]',
                isActive
                  ? 'bg-terminal-600 text-terminal-50'
                  : 'text-terminal-400 hover:text-terminal-100 disabled:cursor-not-allowed disabled:opacity-40',
              ].join(' ')}
            >
              {tab.label}
            </button>
          )
        })}
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

      {/* Library picker */}
      {isLibrary && (
        <LibraryPicker
          programId={programId}
          selectedId={sourceType === '_library_ref' ? config.source.value : ''}
          onSelect={(entryId) => update({ source: { type: '_library_ref', value: entryId } })}
        />
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

interface LibraryPickerProps {
  programId: string
  selectedId: string
  onSelect: (entryId: string) => void
}

function LibraryPicker({ programId, selectedId, onSelect }: LibraryPickerProps) {
  const [searchText, setSearchText] = useState('')
  const { data: results, isLoading } = useKnowledgeSearch(programId, searchText)

  const entries = results?.map((r) => r.entry) ?? []
  const selectedEntry = entries.find((e) => e.id === selectedId)

  if (!programId) {
    return (
      <AtFieldWarning message="Save the program first to browse the knowledge library" />
    )
  }

  return (
    <div className="space-y-2">
      {/* Search input */}
      <div className="relative">
        <Search size={11} className="absolute left-2 top-1/2 -translate-y-1/2 text-terminal-400" />
        <input
          type="text"
          placeholder="Search entries…"
          value={searchText}
          onChange={(e) => setSearchText(e.target.value)}
          className={[inputClass, 'pl-6'].join(' ')}
          data-testid="library-search"
        />
      </div>

      {/* Selected entry badge */}
      {selectedId && selectedEntry && (
        <div className="rounded border border-at-field-700 bg-at-field-950/40 px-2 py-1 text-[11px] text-at-field-300">
          Selected: <span className="font-medium">{selectedEntry.title}</span>
        </div>
      )}
      {selectedId && !selectedEntry && !isLoading && (
        <div className="rounded border border-warn-amber-700 bg-warn-amber-950/40 px-2 py-1 text-[11px] text-warn-amber-400">
          Entry ID: {selectedId}
        </div>
      )}

      {/* Results list */}
      <div className="max-h-48 overflow-y-auto rounded border border-terminal-600 bg-terminal-900">
        {isLoading && (
          <p className="px-3 py-2 text-[11px] text-terminal-400">Loading…</p>
        )}
        {!isLoading && entries.length === 0 && (
          <p className="px-3 py-2 text-[11px] text-terminal-400">
            {searchText ? 'No entries match your search' : 'No knowledge entries for this program'}
          </p>
        )}
        {entries.map((entry) => (
          <button
            key={entry.id}
            onClick={() => onSelect(entry.id)}
            className={[
              'w-full px-3 py-2 text-left text-[11px] transition-colors duration-[150ms]',
              entry.id === selectedId
                ? 'bg-at-field-900/60 text-at-field-200'
                : 'text-terminal-200 hover:bg-terminal-700',
            ].join(' ')}
            data-testid={`entry-${entry.id}`}
          >
            <span className="block truncate font-medium">{entry.title}</span>
            <span className="block truncate text-terminal-400">{entry.category}</span>
          </button>
        ))}
      </div>

      {selectedId && !selectedEntry && !isLoading && entries.length > 0 && (
        <AtFieldWarning message="Selected entry not found in search results — it may have been deleted" />
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
