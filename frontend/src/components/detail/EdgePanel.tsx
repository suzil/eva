import { ArrowRight } from 'lucide-react'
import { useCanvasStore } from '../../store/canvasStore'
import { NODE_TYPE_META } from '../nodes/constants'

export function EdgePanel() {
  const selectedEdgeId = useCanvasStore((s) => s.selectedEdgeId)
  const edges = useCanvasStore((s) => s.edges)
  const nodes = useCanvasStore((s) => s.nodes)

  const edge = edges.find((e) => e.id === selectedEdgeId)
  if (!edge) return null

  const sourceNode = nodes.find((n) => n.id === edge.source)
  const targetNode = nodes.find((n) => n.id === edge.target)
  const isResource = edge.type === 'resource'

  return (
    <div className="flex flex-1 flex-col gap-4 overflow-y-auto p-4">
      {/* Edge category badge */}
      <div className="flex items-center gap-2">
        <span
          className={[
            'rounded border px-2 py-0.5 text-[10px] font-semibold uppercase tracking-wider',
            isResource
              ? 'border-terminal-500 text-terminal-300'
              : 'border-magi-blue-800 text-magi-blue-400',
          ].join(' ')}
        >
          {isResource ? 'Resource' : 'Data'}
        </span>
        <span className="text-[11px] text-terminal-400">
          {isResource ? 'Static capability binding' : 'Runtime message flow'}
        </span>
      </div>

      {/* Source → Target */}
      <div className="rounded-lg border border-terminal-500 bg-terminal-800/50 p-3">
        <NodePortRow
          label="Source"
          nodeName={sourceNode?.data.label ?? edge.source}
          nodeType={sourceNode?.type ?? ''}
          portName={edge.sourceHandle ?? '—'}
        />
        <div className="my-2 flex items-center justify-center">
          <ArrowRight className="h-3.5 w-3.5 text-terminal-500" />
        </div>
        <NodePortRow
          label="Target"
          nodeName={targetNode?.data.label ?? edge.target}
          nodeType={targetNode?.type ?? ''}
          portName={edge.targetHandle ?? '—'}
        />
      </div>
    </div>
  )
}

function NodePortRow({
  label,
  nodeName,
  nodeType,
  portName,
}: {
  label: string
  nodeName: string
  nodeType: string
  portName: string
}) {
  const meta = NODE_TYPE_META[nodeType]
  const Icon = meta?.icon

  return (
    <div className="flex items-center justify-between">
      <div className="flex items-center gap-1.5">
        <span className="w-10 text-[10px] text-terminal-400">{label}</span>
        {Icon && (
          <Icon size={11} style={{ color: meta.accentColor }} className="shrink-0" />
        )}
        <span className="text-[11px] font-medium text-terminal-100">{nodeName}</span>
      </div>
      <span className="rounded bg-terminal-700 px-1.5 py-0.5 font-mono text-[10px] text-terminal-300">
        {portName}
      </span>
    </div>
  )
}
