import { useState, useRef, useEffect } from 'react'
import { Save, Loader2 } from 'lucide-react'
import { useCanvasStore } from '../../store/canvasStore'
import { useUiStore } from '../../store/uiStore'
import { useSaveGraph } from '../../api/hooks'
import { NODE_TYPE_META } from '../nodes/constants'
import { AgentForm } from './forms/AgentForm'
import { KnowledgeForm } from './forms/KnowledgeForm'
import { ConnectorForm } from './forms/ConnectorForm'
import { ActionForm } from './forms/ActionForm'
import { TriggerForm } from './forms/TriggerForm'
import type { NodeType } from '../../types'

// ---------------------------------------------------------------------------
// Access Summary — read-only, shown only for Agent nodes
// ---------------------------------------------------------------------------

function AccessSummary({ nodeId }: { nodeId: string }) {
  const edges = useCanvasStore((s) => s.edges)
  const nodes = useCanvasStore((s) => s.nodes)

  const resourceEdges = edges.filter(
    (e) => e.target === nodeId && e.type === 'resource',
  )

  if (resourceEdges.length === 0) return null

  const knowledge: string[] = []
  const tools: string[] = []

  for (const e of resourceEdges) {
    const src = nodes.find((n) => n.id === e.source)
    if (!src) continue
    const srcType = src.data.nodeType.type
    if (srcType === 'knowledge') knowledge.push(src.data.label)
    if (srcType === 'connector') tools.push(src.data.label)
  }

  if (knowledge.length === 0 && tools.length === 0) return null

  return (
    <div className="mt-4 rounded-lg border border-gray-800 bg-gray-900/40 p-3">
      <p className="mb-1.5 text-[10px] font-semibold uppercase tracking-wider text-gray-600">
        Access Summary
      </p>
      {knowledge.length > 0 && (
        <p className="text-[11px] text-gray-400">
          <span className="text-gray-600">Knowledge: </span>
          {knowledge.join(', ')}
        </p>
      )}
      {tools.length > 0 && (
        <p className="text-[11px] text-gray-400">
          <span className="text-gray-600">Tools: </span>
          {tools.join(', ')}
        </p>
      )}
    </div>
  )
}

// ---------------------------------------------------------------------------
// NodePanel
// ---------------------------------------------------------------------------

export function NodePanel() {
  const selectedNodeId = useCanvasStore((s) => s.selectedNodeId)
  const nodes = useCanvasStore((s) => s.nodes)
  const updateNodeLabel = useCanvasStore((s) => s.updateNodeLabel)
  const updateNodeConfig = useCanvasStore((s) => s.updateNodeConfig)
  const buildGraph = useCanvasStore((s) => s.buildGraph)
  const markClean = useCanvasStore((s) => s.markClean)

  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const saveGraph = useSaveGraph(selectedProgramId ?? '')

  const node = nodes.find((n) => n.id === selectedNodeId)

  // Editable label state — synced from node data
  const [labelDraft, setLabelDraft] = useState(node?.data.label ?? '')
  const labelInputRef = useRef<HTMLInputElement>(null)

  useEffect(() => {
    setLabelDraft(node?.data.label ?? '')
  }, [node?.data.label, selectedNodeId])

  if (!node) return null

  const typeKey = node.data.nodeType.type
  const meta = NODE_TYPE_META[typeKey]
  const Icon = meta?.icon

  const handleLabelBlur = () => {
    const trimmed = labelDraft.trim()
    if (trimmed && trimmed !== node.data.label) {
      updateNodeLabel(node.id, trimmed)
    } else {
      setLabelDraft(node.data.label)
    }
  }

  const handleLabelKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Enter') labelInputRef.current?.blur()
    if (e.key === 'Escape') {
      setLabelDraft(node.data.label)
      labelInputRef.current?.blur()
    }
  }

  const handleConfigChange = (config: NodeType['config']) => {
    updateNodeConfig(node.id, config)
  }

  const handleSave = () => {
    if (!selectedProgramId) return
    saveGraph.mutate(buildGraph(), {
      onSuccess: () => markClean(),
    })
  }

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Header: type icon + editable label + Save */}
      <div className="flex shrink-0 items-center gap-2 border-b border-gray-800 px-3 py-2">
        {Icon && meta && (
          <div
            className={`flex h-5 w-5 shrink-0 items-center justify-center rounded ${meta.accentClass}`}
          >
            <Icon size={11} className="text-white" />
          </div>
        )}
        <input
          ref={labelInputRef}
          value={labelDraft}
          onChange={(e) => setLabelDraft(e.target.value)}
          onBlur={handleLabelBlur}
          onKeyDown={handleLabelKeyDown}
          className="min-w-0 flex-1 bg-transparent text-sm font-semibold text-white outline-none ring-0 placeholder:text-gray-600 hover:bg-gray-800/50 focus:bg-gray-800/50 rounded px-1 -mx-1 py-0.5"
          aria-label="Node label"
        />
        <button
          onClick={handleSave}
          disabled={!selectedProgramId || saveGraph.isPending}
          title={selectedProgramId ? 'Save graph to backend' : 'Select a program first'}
          className="flex items-center gap-1 rounded border border-gray-700 bg-gray-800 px-2 py-0.5 text-[11px] text-gray-300 hover:bg-gray-700 hover:text-white disabled:cursor-not-allowed disabled:opacity-40 transition-colors"
        >
          {saveGraph.isPending ? (
            <Loader2 size={11} className="animate-spin" />
          ) : (
            <Save size={11} />
          )}
          Save
        </button>
      </div>

      {/* Save feedback */}
      {saveGraph.isSuccess && (
        <div className="shrink-0 border-b border-green-900/60 bg-green-950/40 px-3 py-1 text-[10px] text-green-400">
          Saved successfully
        </div>
      )}
      {saveGraph.isError && (
        <div className="shrink-0 border-b border-red-900/60 bg-red-950/40 px-3 py-1 text-[10px] text-red-400">
          Save failed — {(saveGraph.error as Error).message}
        </div>
      )}

      {/* Config form */}
      <div className="flex-1 overflow-y-auto">
        <div className="p-3">
          {typeKey === 'agent' && (
            <>
              <AgentForm
                config={node.data.nodeType.config as Parameters<typeof AgentForm>[0]['config']}
                onChange={handleConfigChange}
              />
              <AccessSummary nodeId={node.id} />
            </>
          )}
          {typeKey === 'knowledge' && (
            <KnowledgeForm
              config={node.data.nodeType.config as Parameters<typeof KnowledgeForm>[0]['config']}
              onChange={handleConfigChange}
            />
          )}
          {typeKey === 'connector' && (
            <ConnectorForm
              config={node.data.nodeType.config as Parameters<typeof ConnectorForm>[0]['config']}
              onChange={handleConfigChange}
            />
          )}
          {typeKey === 'action' && (
            <ActionForm
              config={node.data.nodeType.config as Parameters<typeof ActionForm>[0]['config']}
              onChange={handleConfigChange}
            />
          )}
          {typeKey === 'trigger' && (
            <TriggerForm
              config={node.data.nodeType.config as Parameters<typeof TriggerForm>[0]['config']}
              onChange={handleConfigChange}
            />
          )}
        </div>
      </div>
    </div>
  )
}
