import { useState } from 'react'
import type { PromptTemplate } from '../../types'
import { TemplateEditor } from './TemplateEditor'

// Split body text on `{{token}}` patterns and render token spans in orange.
function BodyWithHighlights({ body }: { body: string }) {
  const parts = body.split(/(\{\{[^}]+\}\})/)
  return (
    <pre className="whitespace-pre-wrap break-words font-mono text-[11px] leading-relaxed text-terminal-200">
      {parts.map((part, i) =>
        /^\{\{[^}]+\}\}$/.test(part) ? (
          <span key={i} className="text-at-field-400">
            {part}
          </span>
        ) : (
          part
        ),
      )}
    </pre>
  )
}

interface Props {
  template: PromptTemplate | null
  onInsert?: (body: string) => void
}

export function TemplatePreview({ template, onInsert }: Props) {
  const [editing, setEditing] = useState(false)

  if (!template) {
    return (
      <div className="flex flex-1 flex-col items-center justify-center gap-1 p-6">
        <p className="text-[11px] text-terminal-500">Select a template to preview</p>
      </div>
    )
  }

  if (editing) {
    return (
      <TemplateEditor
        template={template}
        onSave={() => setEditing(false)}
        onCancel={() => setEditing(false)}
      />
    )
  }

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Header */}
      <div className="flex-shrink-0 border-b border-terminal-700 px-4 py-3">
        <div className="flex items-start justify-between gap-2">
          <div className="min-w-0">
            <p className="truncate text-[13px] font-semibold text-terminal-50">{template.name}</p>
            <p className="mt-0.5 text-[11px] text-terminal-400">{template.description}</p>
          </div>
          {!template.builtIn && (
            <button
              onClick={() => setEditing(true)}
              className="flex-shrink-0 rounded border border-terminal-600 px-2 py-1 text-[10px] text-terminal-300 transition-colors duration-[150ms] hover:border-terminal-400 hover:text-terminal-100"
            >
              Edit
            </button>
          )}
        </div>

        {/* Tags */}
        {template.tags.length > 0 && (
          <div className="mt-2 flex flex-wrap gap-1">
            {template.tags.map((tag) => (
              <span
                key={tag}
                className="rounded bg-terminal-700 px-1.5 py-0.5 font-mono text-[9px] text-terminal-400"
              >
                {tag}
              </span>
            ))}
          </div>
        )}
      </div>

      {/* Body */}
      <div className="flex-1 overflow-y-auto px-4 py-3">
        <BodyWithHighlights body={template.body} />

        {/* Variable list */}
        {template.variables.length > 0 && (
          <div className="mt-4 border-t border-terminal-700 pt-3">
            <p className="mb-2 font-display text-[10px] uppercase tracking-widest text-terminal-400">
              Variables
            </p>
            <div className="space-y-2">
              {template.variables.map((v) => (
                <div key={v.name} className="rounded border border-terminal-700 bg-terminal-800 px-3 py-2">
                  <div className="flex items-center gap-1.5">
                    <span className="font-mono text-[11px] text-at-field-400">{`{{${v.name}}}`}</span>
                    {v.required && (
                      <span className="rounded bg-nerv-red-950 px-1 py-0.5 font-display text-[9px] uppercase tracking-widest text-nerv-red-400">
                        required
                      </span>
                    )}
                    {v.defaultValue !== undefined && (
                      <span className="ml-auto truncate font-mono text-[10px] text-terminal-500">
                        default: {v.defaultValue}
                      </span>
                    )}
                  </div>
                  {v.description && (
                    <p className="mt-0.5 text-[10px] text-terminal-400">{v.description}</p>
                  )}
                </div>
              ))}
            </div>
          </div>
        )}
      </div>

      {/* Footer */}
      {onInsert && (
        <div className="flex-shrink-0 border-t border-terminal-700 px-4 py-2.5">
          <button
            onClick={() => onInsert(template.body)}
            className="w-full rounded bg-at-field-600 px-3 py-1.5 text-[11px] font-medium text-terminal-950 transition-colors duration-[150ms] hover:bg-at-field-500"
          >
            Use Template
          </button>
        </div>
      )}
    </div>
  )
}
