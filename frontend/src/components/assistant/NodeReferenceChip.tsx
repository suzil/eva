import { NODE_TYPE_META } from '../../constants/nodeConstants'
import { useCanvasStore } from '../../store/canvasStore'
import { useUiStore } from '../../store/uiStore'

interface NodeReferenceChipProps {
  nodeId: string
  label: string
  nodeType?: string
  /** When true, renders a larger context chip at the top of AssistantPanel */
  context?: boolean
}

export function NodeReferenceChip({ nodeId, label, nodeType, context = false }: NodeReferenceChipProps) {
  const meta = nodeType ? NODE_TYPE_META[nodeType] : undefined
  const Icon = meta?.icon
  const accentColor = meta?.accentColor ?? '#6B7280'

  const setHoveredNodeId = useCanvasStore((s) => s.setHoveredNodeId)
  const setSelectedNode = useCanvasStore((s) => s.setSelectedNode)
  const setDetailPanelTab = useUiStore((s) => s.setDetailPanelTab)

  if (context) {
    return (
      <div
        className="flex items-center gap-1.5 rounded px-2 py-1 text-xs"
        style={{ backgroundColor: `${accentColor}20`, border: `1px solid ${accentColor}40` }}
        onMouseEnter={() => setHoveredNodeId(nodeId)}
        onMouseLeave={() => setHoveredNodeId(null)}
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
      className="inline-flex cursor-pointer items-center gap-1 rounded px-1.5 py-0.5 text-xs"
      style={{ backgroundColor: `${accentColor}20`, color: accentColor }}
      onMouseEnter={() => setHoveredNodeId(nodeId)}
      onMouseLeave={() => setHoveredNodeId(null)}
      onClick={() => {
        setSelectedNode(nodeId)
        setDetailPanelTab('inspector')
      }}
    >
      {Icon && <Icon className="h-3 w-3 flex-shrink-0" />}
      {label}
    </span>
  )
}
