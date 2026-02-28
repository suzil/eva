import { NODE_TYPE_META } from '../../constants/nodeConstants'

interface NodeReferenceChipProps {
  nodeId: string
  label: string
  nodeType?: string
  /** When true, renders a larger context chip at the top of AssistantPanel */
  context?: boolean
}

export function NodeReferenceChip({ label, nodeType, context = false }: NodeReferenceChipProps) {
  const meta = nodeType ? NODE_TYPE_META[nodeType] : undefined
  const Icon = meta?.icon
  const accentColor = meta?.accentColor ?? '#6B7280'

  if (context) {
    return (
      <div
        className="flex items-center gap-1.5 rounded px-2 py-1 text-xs"
        style={{ backgroundColor: `${accentColor}20`, border: `1px solid ${accentColor}40` }}
      >
        {Icon && <Icon className="h-3 w-3 flex-shrink-0" style={{ color: accentColor }} />}
        <span className="font-display uppercase tracking-widest" style={{ color: accentColor }}>
          {label}
        </span>
      </div>
    )
  }

  return (
    <span
      className="inline-flex items-center gap-1 rounded px-1.5 py-0.5 text-xs"
      style={{ backgroundColor: `${accentColor}20`, color: accentColor }}
    >
      {Icon && <Icon className="h-3 w-3 flex-shrink-0" />}
      {label}
    </span>
  )
}
