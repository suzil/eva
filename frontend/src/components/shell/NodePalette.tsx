import { NODE_TYPE_META } from '../nodes/constants'

// ---------------------------------------------------------------------------
// Node type descriptions shown in the palette
// ---------------------------------------------------------------------------

const NODE_DESCRIPTIONS: Record<string, string> = {
  agent: 'LLM reasoning — invokes tools, produces output',
  knowledge: 'Information source — context for agents',
  connector: 'External system — exposes tools',
  action: 'Deterministic transform — no LLM',
  trigger: 'Entry point — starts a run',
}

// Canonical display order
const NODE_TYPE_ORDER = ['trigger', 'agent', 'knowledge', 'connector', 'action'] as const

// ---------------------------------------------------------------------------
// NodePalette
// ---------------------------------------------------------------------------

export function NodePalette() {
  return (
    <div className="flex flex-1 flex-col overflow-y-auto p-2 gap-1">
      <p className="px-1 py-1 font-display text-xs uppercase tracking-widest text-terminal-300">
        Drag onto canvas
      </p>
      {NODE_TYPE_ORDER.map((key) => {
        const meta = NODE_TYPE_META[key]
        if (!meta) return null
        const Icon = meta.icon

        return (
          <div
            key={key}
            draggable
            onDragStart={(e) => {
              e.dataTransfer.setData('application/eva-node-type', key)
              e.dataTransfer.effectAllowed = 'move'
            }}
            className="flex cursor-grab items-start gap-2.5 rounded-md border border-terminal-500/60 bg-terminal-700/50 px-2.5 py-2 transition-colors hover:border-terminal-400 hover:bg-terminal-700 active:cursor-grabbing"
          >
            {/* Icon with accent background */}
            <div
              className={`mt-0.5 flex h-6 w-6 flex-shrink-0 items-center justify-center rounded ${meta.accentClass}/20`}
            >
              <Icon
                className="h-3.5 w-3.5"
                style={{ color: meta.accentColor }}
              />
            </div>

            {/* Label + description */}
            <div className="min-w-0">
              <p className="text-xs font-medium text-terminal-100">{meta.label}</p>
              <p className="mt-0.5 text-[10px] leading-tight text-terminal-300">
                {NODE_DESCRIPTIONS[key]}
              </p>
            </div>
          </div>
        )
      })}
      {/* Port type legend */}
      <div className="mt-2 border-t border-terminal-500/40 pt-2 px-1">
        <p className="mb-1.5 text-[10px] uppercase tracking-widest text-terminal-400">
          Port types
        </p>
        <div className="flex flex-col gap-1.5">
          <div className="flex items-center gap-2 text-[10px] text-terminal-300">
            <div className="h-3 w-3 flex-shrink-0 rounded-full border-2 border-terminal-400 bg-terminal-500" />
            <span>
              <span className="text-terminal-100">Data</span>
              {' — '}text or JSON values
            </span>
          </div>
          <div className="flex items-center gap-2 text-[10px] text-terminal-300">
            <div className="h-3 w-3 flex-shrink-0 rotate-45 border-2 border-terminal-400 bg-terminal-500" />
            <span>
              <span className="text-terminal-100">Resource</span>
              {' — '}tools or context
            </span>
          </div>
        </div>
      </div>
    </div>
  )
}
