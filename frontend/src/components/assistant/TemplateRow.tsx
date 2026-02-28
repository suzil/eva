import type { PromptTemplate, TemplateCategory } from '../../types'

const CATEGORY_BADGE: Record<TemplateCategory, { label: string; className: string }> = {
  summarizer: { label: 'summarizer', className: 'bg-at-field-900 text-at-field-300' },
  reviewer:   { label: 'reviewer',   className: 'bg-magi-blue-900 text-magi-blue-300' },
  classifier: { label: 'classifier', className: 'bg-eva-green-900 text-eva-green-400' },
  extractor:  { label: 'extractor',  className: 'bg-nerv-red-950 text-nerv-red-400' },
  formatter:  { label: 'formatter',  className: 'bg-terminal-700 text-terminal-300' },
  analyst:    { label: 'analyst',    className: 'bg-warn-amber-950 text-warn-amber-400' },
  custom:     { label: 'custom',     className: 'bg-terminal-800 text-terminal-400' },
}

interface Props {
  template: PromptTemplate
  isSelected: boolean
  onSelect: () => void
  onInsert?: () => void
}

export function TemplateRow({ template, isSelected, onSelect, onInsert }: Props) {
  const badge = CATEGORY_BADGE[template.category] ?? {
    label: template.category,
    className: 'bg-terminal-700 text-terminal-400',
  }

  return (
    <button
      onClick={onSelect}
      onDoubleClick={() => onInsert?.()}
      className={[
        'w-full px-3 py-2 text-left transition-colors duration-[150ms]',
        isSelected ? 'bg-at-field-900/60' : 'hover:bg-terminal-700/60',
      ].join(' ')}
      data-testid={`template-row-${template.id}`}
    >
      <div className="flex min-w-0 items-center gap-1.5">
        <span
          className={[
            'flex-1 truncate text-[11px] font-medium',
            isSelected ? 'text-at-field-200' : 'text-terminal-100',
          ].join(' ')}
        >
          {template.name}
        </span>

        {template.builtIn && (
          <span className="flex-shrink-0 rounded bg-terminal-700 px-1.5 py-0.5 font-display text-[9px] uppercase tracking-widest text-terminal-400">
            built-in
          </span>
        )}
      </div>

      <div className="mt-0.5 flex items-center gap-1.5">
        <span
          className={`flex-shrink-0 rounded px-1.5 py-0.5 font-display text-[9px] uppercase tracking-widest ${badge.className}`}
        >
          {badge.label}
        </span>
        <span className="truncate text-[10px] text-terminal-400">{template.description}</span>
      </div>
    </button>
  )
}
