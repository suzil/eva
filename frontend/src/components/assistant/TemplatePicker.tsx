import { useEffect, useRef, useState } from 'react'
import { Search, X } from 'lucide-react'
import { useTemplates } from '../../api/hooks'
import type { PromptTemplate, TemplateCategory } from '../../types'
import { TemplateRow } from './TemplateRow'
import { TemplatePreview } from './TemplatePreview'
import { TemplateEditor } from './TemplateEditor'
import { useFocusTrap } from '../../hooks/useFocusTrap'

const ALL_CATEGORIES: TemplateCategory[] = [
  'summarizer',
  'reviewer',
  'classifier',
  'extractor',
  'formatter',
  'analyst',
  'custom',
]

function matchSearch(t: PromptTemplate, query: string): boolean {
  if (!query) return true
  const q = query.toLowerCase()
  return (
    t.name.toLowerCase().includes(q) ||
    t.description.toLowerCase().includes(q) ||
    t.tags.some((tag) => tag.toLowerCase().includes(q))
  )
}

interface Props {
  open: boolean
  onClose: () => void
  onInsert: (body: string) => void
}

/**
 * Outer wrapper: renders nothing when closed so the inner component (which
 * calls useTemplates) is never mounted — and therefore never requires a
 * QueryClient — until the picker is actually opened.
 */
export function TemplatePicker({ open, onClose, onInsert }: Props) {
  if (!open) return null
  return <TemplatePickerInner onClose={onClose} onInsert={onInsert} />
}

interface InnerProps {
  onClose: () => void
  onInsert: (body: string) => void
}

function TemplatePickerInner({ onClose, onInsert }: InnerProps) {
  const [search, setSearch] = useState('')
  const [categoryFilter, setCategoryFilter] = useState<TemplateCategory | 'all'>('all')
  const [selectedId, setSelectedId] = useState<string | null>(null)
  const [showEditor, setShowEditor] = useState(false)
  const [editingTemplate, setEditingTemplate] = useState<PromptTemplate | undefined>(undefined)

  const searchRef = useRef<HTMLInputElement>(null)
  const dialogRef = useRef<HTMLDivElement>(null)
  const prevFocusRef = useRef<Element | null>(null)
  const { data: templates = [], isLoading } = useTemplates()

  // Save focus before mount; restore it when the picker is closed/unmounted
  useEffect(() => {
    prevFocusRef.current = document.activeElement
    return () => {
      if (prevFocusRef.current instanceof HTMLElement) {
        prevFocusRef.current.focus()
      }
    }
  }, [])

  // Focus search on mount (manages its own initial focus; useFocusTrap skips auto-focus)
  useEffect(() => {
    setTimeout(() => searchRef.current?.focus(), 50)
  }, [])

  // Trap Tab inside dialog (skip initial auto-focus; we manage it above)
  useFocusTrap(dialogRef, true, { skipInitialFocus: true })

  // Close on Escape
  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onClose()
    }
    document.addEventListener('keydown', handler)
    return () => document.removeEventListener('keydown', handler)
  }, [onClose])

  const filtered = templates.filter(
    (t) => (categoryFilter === 'all' || t.category === categoryFilter) && matchSearch(t, search),
  )

  const selectedTemplate = templates.find((t) => t.id === selectedId) ?? null

  const handleInsert = (body: string) => {
    onInsert(body)
    onClose()
  }

  const handleEditorSave = (saved: PromptTemplate) => {
    setSelectedId(saved.id)
    setShowEditor(false)
    setEditingTemplate(undefined)
  }

  return (
    <div
      className="fixed inset-0 z-50 flex items-center justify-center bg-terminal-950/80"
      onClick={(e) => {
        if (e.target === e.currentTarget) onClose()
      }}
    >
      <div
        ref={dialogRef}
        role="dialog"
        aria-modal="true"
        aria-labelledby="template-picker-title"
        className="flex h-[600px] w-[760px] flex-col overflow-hidden rounded-lg border border-terminal-600 bg-terminal-900 shadow-2xl"
      >
        {/* Header */}
        <div className="flex flex-shrink-0 items-center justify-between border-b border-terminal-700 px-4 py-3">
          <p
            id="template-picker-title"
            className="font-display text-[11px] uppercase tracking-widest text-terminal-300"
          >
            Template Library
          </p>
          <button
            onClick={onClose}
            className="rounded p-1 text-terminal-400 transition-colors hover:text-terminal-100"
            aria-label="Close"
          >
            <X size={14} aria-hidden="true" />
          </button>
        </div>

        {showEditor ? (
          <TemplateEditor
            template={editingTemplate}
            onSave={handleEditorSave}
            onCancel={() => {
              setShowEditor(false)
              setEditingTemplate(undefined)
            }}
          />
        ) : (
          <>
            {/* Search + category filter */}
            <div className="flex-shrink-0 border-b border-terminal-700 px-4 py-2.5">
              <div className="relative mb-2">
                <Search
                  size={11}
                  className="absolute left-2.5 top-1/2 -translate-y-1/2 text-terminal-400"
                  aria-hidden="true"
                />
                {/* Visually hidden label associates the search input for screen readers */}
                <label htmlFor="template-picker-search" className="sr-only">
                  Search templates
                </label>
                <input
                  ref={searchRef}
                  id="template-picker-search"
                  type="text"
                  placeholder="Search templates…"
                  value={search}
                  onChange={(e) => setSearch(e.target.value)}
                  className="w-full rounded border border-terminal-600 bg-terminal-800 py-1.5 pl-7 pr-3 text-[11px] text-terminal-100 placeholder:text-terminal-500 outline-none focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors duration-[150ms]"
                />
              </div>

              {/* Category tabs */}
              <div className="flex gap-1 overflow-x-auto pb-0.5" role="group" aria-label="Filter by category">
                {(['all', ...ALL_CATEGORIES] as const).map((cat) => (
                  <button
                    key={cat}
                    onClick={() => setCategoryFilter(cat)}
                    aria-pressed={categoryFilter === cat}
                    className={[
                      'flex-shrink-0 rounded px-2 py-0.5 font-display text-[9px] uppercase tracking-widest transition-colors duration-[150ms]',
                      categoryFilter === cat
                        ? 'bg-terminal-600 text-terminal-50'
                        : 'text-terminal-400 hover:text-terminal-100',
                    ].join(' ')}
                  >
                    {cat}
                  </button>
                ))}
              </div>
            </div>

            {/* Body: list + preview */}
            <div className="flex flex-1 overflow-hidden">
              {/* Left: template list */}
              <div className="flex w-2/5 flex-col overflow-hidden border-r border-terminal-700">
                <div
                  className="flex-1 overflow-y-auto"
                  role="listbox"
                  aria-label="Templates"
                >
                  {isLoading && (
                    <p className="px-3 py-4 text-center text-[11px] text-terminal-500">
                      Loading…
                    </p>
                  )}
                  {!isLoading && filtered.length === 0 && (
                    <p className="px-3 py-4 text-center text-[11px] text-terminal-500">
                      No templates match your search
                    </p>
                  )}
                  {filtered.map((t) => (
                    <TemplateRow
                      key={t.id}
                      template={t}
                      isSelected={selectedId === t.id}
                      onSelect={() => setSelectedId(t.id)}
                      onInsert={() => handleInsert(t.body)}
                    />
                  ))}
                </div>

                {/* New template button */}
                <div className="flex-shrink-0 border-t border-terminal-700 px-3 py-2">
                  <button
                    onClick={() => {
                      setEditingTemplate(undefined)
                      setShowEditor(true)
                    }}
                    className="w-full rounded border border-terminal-600 py-1.5 text-[10px] text-terminal-400 transition-colors duration-[150ms] hover:border-terminal-400 hover:text-terminal-100"
                  >
                    + New Template
                  </button>
                </div>
              </div>

              {/* Right: preview */}
              <div className="flex w-3/5 flex-col overflow-hidden">
                <TemplatePreview template={selectedTemplate} onInsert={handleInsert} />
              </div>
            </div>
          </>
        )}
      </div>
    </div>
  )
}
