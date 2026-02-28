import { useState } from 'react'
import { Loader2 } from 'lucide-react'
import type { PromptTemplate, TemplateCategory } from '../../types'
import { useCreateTemplate, usePatchTemplate } from '../../api/hooks'

const CATEGORIES: TemplateCategory[] = [
  'summarizer',
  'reviewer',
  'classifier',
  'extractor',
  'formatter',
  'analyst',
  'custom',
]

interface Props {
  template?: PromptTemplate
  onSave: (saved: PromptTemplate) => void
  onCancel: () => void
}

export function TemplateEditor({ template, onSave, onCancel }: Props) {
  const isEdit = template !== undefined

  const [name, setName] = useState(template?.name ?? '')
  const [description, setDescription] = useState(template?.description ?? '')
  const [category, setCategory] = useState<TemplateCategory>(template?.category ?? 'custom')
  const [body, setBody] = useState(template?.body ?? '')
  const [tagsInput, setTagsInput] = useState(template?.tags.join(', ') ?? '')

  const createMutation = useCreateTemplate()
  const patchMutation = usePatchTemplate(template?.id ?? '')

  const isPending = createMutation.isPending || patchMutation.isPending
  const error = createMutation.error ?? patchMutation.error

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    if (!name.trim() || !body.trim()) return

    const tags = tagsInput
      .split(',')
      .map((t) => t.trim())
      .filter(Boolean)

    if (isEdit) {
      patchMutation.mutate(
        { name, description, category, body, tags },
        { onSuccess: (saved) => onSave(saved) },
      )
    } else {
      createMutation.mutate(
        { name, description, category, body, tags, variables: [] },
        { onSuccess: (saved) => onSave(saved) },
      )
    }
  }

  return (
    <form onSubmit={handleSubmit} className="flex flex-1 flex-col overflow-hidden">
      <div className="flex-shrink-0 border-b border-terminal-700 px-4 py-3">
        <p className="font-display text-[11px] uppercase tracking-widest text-terminal-300">
          {isEdit ? 'Edit template' : 'New template'}
        </p>
      </div>

      <div className="flex-1 space-y-3 overflow-y-auto px-4 py-3">
        {/* Name */}
        <div>
          <label className="mb-1 block text-[11px] font-medium text-terminal-200">Name</label>
          <input
            value={name}
            onChange={(e) => setName(e.target.value)}
            required
            disabled={isPending}
            placeholder="e.g. Weekly Summarizer"
            className={inputClass}
          />
        </div>

        {/* Description */}
        <div>
          <label className="mb-1 block text-[11px] font-medium text-terminal-200">Description</label>
          <input
            value={description}
            onChange={(e) => setDescription(e.target.value)}
            disabled={isPending}
            placeholder="One-line description"
            className={inputClass}
          />
        </div>

        {/* Category */}
        <div>
          <label className="mb-1 block text-[11px] font-medium text-terminal-200">Category</label>
          <select
            value={category}
            onChange={(e) => setCategory(e.target.value as TemplateCategory)}
            disabled={isPending}
            className={inputClass}
          >
            {CATEGORIES.map((c) => (
              <option key={c} value={c}>
                {c}
              </option>
            ))}
          </select>
        </div>

        {/* Body */}
        <div>
          <label className="mb-1 block text-[11px] font-medium text-terminal-200">
            Prompt body
          </label>
          <textarea
            value={body}
            onChange={(e) => setBody(e.target.value)}
            required
            rows={10}
            disabled={isPending}
            placeholder={'Use {{variable}} for dynamic values…'}
            className={`${inputClass} resize-none font-mono`}
          />
          <p className="mt-1 font-mono text-[10px] text-terminal-500">
            Use {'{{variable}}'} tokens for dynamic binding at runtime.
          </p>
        </div>

        {/* Tags */}
        <div>
          <label className="mb-1 block text-[11px] font-medium text-terminal-200">
            Tags (comma-separated)
          </label>
          <input
            value={tagsInput}
            onChange={(e) => setTagsInput(e.target.value)}
            disabled={isPending}
            placeholder="e.g. weekly, linear, reports"
            className={inputClass}
          />
        </div>

        {error && (
          <p className="rounded border border-nerv-red-700 bg-nerv-red-950/40 px-2 py-1.5 text-[10px] text-nerv-red-400">
            {error instanceof Error ? error.message : 'Save failed'}
          </p>
        )}
      </div>

      {/* Footer */}
      <div className="flex flex-shrink-0 items-center justify-end gap-2 border-t border-terminal-700 px-4 py-2.5">
        <button
          type="button"
          onClick={onCancel}
          disabled={isPending}
          className="rounded border border-terminal-600 px-3 py-1.5 text-[11px] text-terminal-300 transition-colors duration-[150ms] hover:border-terminal-400 hover:text-terminal-100 disabled:opacity-50"
        >
          Cancel
        </button>
        <button
          type="submit"
          disabled={isPending || !name.trim() || !body.trim()}
          className="flex items-center gap-1.5 rounded bg-at-field-600 px-3 py-1.5 text-[11px] font-medium text-terminal-950 transition-colors duration-[150ms] hover:bg-at-field-500 disabled:opacity-50"
        >
          {isPending && <Loader2 size={11} className="animate-spin" />}
          {isEdit ? 'Save changes' : 'Create template'}
        </button>
      </div>
    </form>
  )
}

const inputClass =
  'w-full rounded border border-terminal-500 bg-terminal-700 px-2 py-1.5 text-[11px] text-terminal-100 outline-none placeholder:text-terminal-500 focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors duration-[150ms] disabled:opacity-60'
