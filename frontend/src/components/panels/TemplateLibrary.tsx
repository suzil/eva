import { useState } from 'react'
import { Search } from 'lucide-react'
import { useDeleteTemplate, useTemplates } from '../../api/hooks'
import type { PromptTemplate, TemplateCategory } from '../../types'
import { TemplateRow } from '../assistant/TemplateRow'
import { TemplatePreview } from '../assistant/TemplatePreview'
import { TemplateEditor } from '../assistant/TemplateEditor'

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

export function TemplateLibrary() {
  const [search, setSearch] = useState('')
  const [categoryFilter, setCategoryFilter] = useState<TemplateCategory | 'all'>('all')
  const [selectedId, setSelectedId] = useState<string | null>(null)
  const [showEditor, setShowEditor] = useState(false)
  const [editingTemplate, setEditingTemplate] = useState<PromptTemplate | undefined>(undefined)

  const { data: templates = [], isLoading } = useTemplates()
  const deleteMutation = useDeleteTemplate()

  const filtered = templates.filter(
    (t) => (categoryFilter === 'all' || t.category === categoryFilter) && matchSearch(t, search),
  )

  const selectedTemplate = templates.find((t) => t.id === selectedId) ?? null

  const handleEditorSave = (saved: PromptTemplate) => {
    setSelectedId(saved.id)
    setShowEditor(false)
    setEditingTemplate(undefined)
  }

  const handleDelete = (id: string) => {
    deleteMutation.mutate(id, {
      onSuccess: () => {
        if (selectedId === id) setSelectedId(null)
      },
    })
  }

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Search */}
      <div className="flex-shrink-0 border-b border-terminal-700 px-2 py-1.5">
        <div className="relative">
          <Search
            size={11}
            className="absolute left-2.5 top-1/2 -translate-y-1/2 text-terminal-400"
          />
          <input
            type="text"
            placeholder="Search templates…"
            value={search}
            onChange={(e) => setSearch(e.target.value)}
            className="w-full rounded border border-terminal-600 bg-terminal-800 py-1 pl-7 pr-2 text-[11px] text-terminal-100 placeholder:text-terminal-500 outline-none focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors duration-[150ms]"
          />
        </div>
      </div>

      {/* Category filter */}
      <div className="flex flex-shrink-0 gap-1 overflow-x-auto border-b border-terminal-700 px-2 py-1.5">
        {(['all', ...ALL_CATEGORIES] as const).map((cat) => (
          <button
            key={cat}
            onClick={() => setCategoryFilter(cat)}
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

      {showEditor ? (
        /* Editor view */
        <TemplateEditor
          template={editingTemplate}
          onSave={handleEditorSave}
          onCancel={() => {
            setShowEditor(false)
            setEditingTemplate(undefined)
          }}
        />
      ) : selectedId && selectedTemplate ? (
        /* Preview view */
        <div className="flex flex-1 flex-col overflow-hidden">
          {/* Back button */}
          <div className="flex flex-shrink-0 items-center justify-between border-b border-terminal-700 px-3 py-1.5">
            <button
              onClick={() => setSelectedId(null)}
              className="text-[10px] text-terminal-400 transition-colors hover:text-terminal-100"
            >
              ← Back
            </button>
            {!selectedTemplate.builtIn && (
              <button
                onClick={() => handleDelete(selectedTemplate.id)}
                disabled={deleteMutation.isPending}
                className="text-[10px] text-nerv-red-400 transition-colors hover:text-nerv-red-300 disabled:opacity-50"
              >
                Delete
              </button>
            )}
          </div>
          <TemplatePreview
            template={selectedTemplate}
          />
        </div>
      ) : (
        /* List view */
        <div className="flex flex-1 flex-col overflow-hidden">
          <div className="flex-1 overflow-y-auto">
            {isLoading && (
              <p className="px-3 py-4 text-center text-[11px] text-terminal-500">Loading…</p>
            )}
            {!isLoading && filtered.length === 0 && (
              <p className="px-3 py-4 text-center text-[11px] text-terminal-500">
                {search || categoryFilter !== 'all'
                  ? 'No templates match your search'
                  : 'No templates yet'}
              </p>
            )}
            {filtered.map((t) => (
              <TemplateRow
                key={t.id}
                template={t}
                isSelected={selectedId === t.id}
                onSelect={() => setSelectedId(t.id)}
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
      )}
    </div>
  )
}
