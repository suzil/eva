import { useCanvasStore } from '../../../store/canvasStore'
import type { PromptVariableBinding } from '../../../types'

interface Props {
  systemPrompt: string
  bindings: Record<string, PromptVariableBinding>
  onChange: (bindings: Record<string, PromptVariableBinding>) => void
  nodeId: string
}

function parseVariables(prompt: string): string[] {
  const seen = new Set<string>()
  const matches = prompt.matchAll(/\{\{(\w+)\}\}/g)
  for (const m of matches) seen.add(m[1])
  return [...seen]
}

export function VariableBindingPanel({ systemPrompt, bindings, onChange, nodeId }: Props) {
  const edges = useCanvasStore((s) => s.edges)
  const nodes = useCanvasStore((s) => s.nodes)

  const variables = parseVariables(systemPrompt)
  if (variables.length === 0) return null

  const dataEdges = edges.filter((e) => e.target === nodeId && e.type === 'data')
  const portOptions = dataEdges.map((e) => {
    const src = nodes.find((n) => n.id === e.source)
    return {
      portId: e.targetHandle as string,
      label: src ? `${src.data.label} → ${e.targetHandle}` : (e.targetHandle as string),
    }
  })

  const getBinding = (varName: string): PromptVariableBinding =>
    bindings[varName] ?? { source: 'literal', value: '' }

  const update = (varName: string, patch: Partial<PromptVariableBinding>) => {
    onChange({ ...bindings, [varName]: { ...getBinding(varName), ...patch } })
  }

  return (
    <div className="rounded border border-terminal-600 bg-terminal-800/40">
      <p className="border-b border-terminal-600 px-2.5 py-1.5 font-display text-[10px] uppercase tracking-widest text-terminal-300">
        Template Variables
      </p>
      <div className="divide-y divide-terminal-700">
        {variables.map((varName) => {
          const binding = getBinding(varName)
          return (
            <div key={varName} className="flex items-center gap-2 px-2.5 py-2">
              {/* Variable name chip */}
              <span className="shrink-0 rounded bg-at-field-900/40 px-1.5 py-0.5 font-mono text-[10px] text-at-field-400">
                {`{{${varName}}}`}
              </span>

              {/* Source mode */}
              <select
                value={binding.source}
                onChange={(e) => {
                  const src = e.target.value as 'port' | 'literal'
                  if (src === 'port') {
                    const firstPort = portOptions[0]
                    update(varName, { source: 'port', portId: firstPort?.portId ?? '', value: undefined })
                  } else {
                    update(varName, { source: 'literal', value: binding.value ?? '', portId: undefined })
                  }
                }}
                className={smallSelectClass}
              >
                <option value="literal">Literal</option>
                <option value="port">Port data</option>
              </select>

              {/* Value / port picker */}
              {binding.source === 'port' ? (
                portOptions.length > 0 ? (
                  <select
                    value={binding.portId ?? portOptions[0]?.portId ?? ''}
                    onChange={(e) => update(varName, { portId: e.target.value })}
                    className={`${smallSelectClass} min-w-0 flex-1 truncate`}
                  >
                    {portOptions.map((opt) => (
                      <option key={opt.portId} value={opt.portId}>
                        {opt.label}
                      </option>
                    ))}
                  </select>
                ) : (
                  <span className="flex-1 text-[10px] italic text-terminal-500">
                    No data ports connected
                  </span>
                )
              ) : (
                <input
                  type="text"
                  value={binding.value ?? ''}
                  placeholder="static value"
                  onChange={(e) => update(varName, { value: e.target.value })}
                  className={smallInputClass}
                />
              )}
            </div>
          )
        })}
      </div>
    </div>
  )
}

const smallSelectClass =
  'rounded border border-terminal-500 bg-terminal-700 px-1.5 py-0.5 text-[11px] text-terminal-100 outline-none focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors duration-[150ms]'

const smallInputClass =
  'min-w-0 flex-1 rounded border border-terminal-500 bg-terminal-700 px-1.5 py-0.5 text-[11px] text-terminal-100 outline-none placeholder:text-terminal-500 focus:border-at-field-500 focus:ring-1 focus:ring-at-field-500/30 transition-colors duration-[150ms]'
