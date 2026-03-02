import { useCallback, useEffect, useMemo, useState } from 'react'
import {
  ReactFlow,
  ReactFlowProvider,
  Controls,
  MiniMap,
  useReactFlow,
  type Node,
  type Edge,
  type OnNodesChange,
  type OnEdgesChange,
  type Connection,
  type IsValidConnection,
  type NodeMouseHandler,
  type EdgeMouseHandler,
  type OnNodeDrag,
} from '@xyflow/react'
import '@xyflow/react/dist/style.css'
import { AlertTriangle, Loader2 } from 'lucide-react'
import { nodeTypes } from '../nodes'
import { NODE_TYPE_META } from '../nodes/constants'
import { NODE_TYPE_COLORS } from '../../constants/nodeConstants'
import { edgeTypes } from '../edges'
import type { EvaNodeData, NodeType } from '../../types'
import { useCanvasStore } from '../../store/canvasStore'
import { useUiStore } from '../../store/uiStore'
import { useProgram, useKnowledgeEntries, useRefreshKnowledge } from '../../api/hooks'
import { GraphPreviewOverlay } from '../canvas/GraphPreviewOverlay'
import { ContextMenu, type MenuItem } from '../canvas/ContextMenu'

// ---------------------------------------------------------------------------
// KnowledgeStalenessBar — shown when any entry scanned_at is >24h old
// ---------------------------------------------------------------------------

const STALE_MS = 24 * 60 * 60 * 1000
const MINIMAP_FALLBACK_COLOR = '#4F5070'  // terminal-400

function KnowledgeStalenessBar({ programId }: { programId: string }) {
  const { data: entries } = useKnowledgeEntries(programId)
  const refresh = useRefreshKnowledge(programId)

  if (!entries || entries.length === 0) return null

  const now = Date.now()
  const isStale = entries.some((e) => now - new Date(e.scannedAt).getTime() > STALE_MS)

  if (!isStale) return null

  return (
    <div className="flex flex-shrink-0 items-center gap-2 border-b border-warn-amber-700 bg-warn-amber-950/60 px-3 py-1.5">
      <AlertTriangle size={12} className="flex-shrink-0 text-warn-amber-400" />
      <span className="flex-1 text-[11px] text-warn-amber-300">
        Knowledge may be stale — some entries have not been refreshed in over 24 hours
      </span>
      <button
        onClick={() => void refresh.mutate()}
        disabled={refresh.isPending}
        className="flex flex-shrink-0 items-center gap-1 rounded border border-warn-amber-700 bg-warn-amber-900/60 px-2 py-0.5 font-display text-[10px] uppercase tracking-widest text-warn-amber-300 transition-colors hover:bg-warn-amber-800 disabled:opacity-50"
      >
        {refresh.isPending && <Loader2 size={10} className="animate-spin" />}
        Refresh All
      </button>
    </div>
  )
}

// ---------------------------------------------------------------------------
// Default configs for newly dropped nodes
// ---------------------------------------------------------------------------

function buildDefaultNodeType(type: string): NodeType {
  switch (type) {
    case 'agent':
      return {
        type: 'agent',
        config: {
          provider: 'openai',
          model: 'gpt-4o',
          systemPrompt: '',
          responseFormat: 'text',
          temperature: 0.7,
          maxIterations: 5,
        },
      }
    case 'knowledge':
      return {
        type: 'knowledge',
        config: {
          source: { type: '_inline_text', value: '' },
          format: 'text',
          refreshPolicy: { type: 'static' },
        },
      }
    case 'connector':
      return {
        type: 'connector',
        config: { system: 'linear', actionFilter: [] },
      }
    case 'action':
      return {
        type: 'action',
        config: { operation: 'template', parameters: {}, errorHandling: { mode: 'fail' } },
      }
    case 'trigger':
    default:
      return {
        type: 'trigger',
        config: { type: 'manual' },
      }
  }
}

function buildDefaultNode(
  id: string,
  type: string,
  position: { x: number; y: number },
): Node<EvaNodeData> {
  const meta = NODE_TYPE_META[type]
  return {
    id,
    type,
    position,
    data: {
      label: meta?.label ?? type,
      nodeType: buildDefaultNodeType(type),
    },
  }
}

// ---------------------------------------------------------------------------
// CanvasInner — must be inside ReactFlowProvider to use useReactFlow
// ---------------------------------------------------------------------------

function CanvasInner() {
  const nodes = useCanvasStore((s) => s.nodes)
  const edges = useCanvasStore((s) => s.edges)
  const nodeStepStates = useCanvasStore((s) => s.nodeStepStates)
  const currentProgramId = useCanvasStore((s) => s.currentProgramId)
  const previewOverlayGraph = useCanvasStore((s) => s.previewOverlayGraph)
  const loadGraph = useCanvasStore((s) => s.loadGraph)
  const applyNodes = useCanvasStore((s) => s.applyNodeChanges)
  const applyEdges = useCanvasStore((s) => s.applyEdgeChanges)
  const addEdgeToStore = useCanvasStore((s) => s.addEdge)
  const addNodeToStore = useCanvasStore((s) => s.addNode)
  const setSelectedNode = useCanvasStore((s) => s.setSelectedNode)
  const setSelectedEdge = useCanvasStore((s) => s.setSelectedEdge)
  const clearSelection = useCanvasStore((s) => s.clearSelection)
  const deleteNodeFromStore = useCanvasStore((s) => s.deleteNode)
  const duplicateNodeFromStore = useCanvasStore((s) => s.duplicateNode)
  const deleteEdgeFromStore = useCanvasStore((s) => s.deleteEdge)
  const snapshot = useCanvasStore((s) => s.snapshot)
  const triggerFitView = useCanvasStore((s) => s.triggerFitView)
  const setTriggerFitView = useCanvasStore((s) => s.setTriggerFitView)
  const { screenToFlowPosition, fitView } = useReactFlow()

  const selectedProgramId = useUiStore((s) => s.selectedProgramId)
  const mode = useUiStore((s) => s.mode)
  const isOperate = mode === 'operate'
  const { data: program } = useProgram(selectedProgramId ?? '')

  useEffect(() => {
    if (program && selectedProgramId && selectedProgramId !== currentProgramId) {
      loadGraph(program.graph, selectedProgramId)
    }
  }, [program, selectedProgramId, currentProgramId, loadGraph])

  // Fire fitView after auto-layout repositions all nodes
  useEffect(() => {
    if (triggerFitView) {
      fitView({ padding: 0.3 })
      setTriggerFitView(false)
    }
  }, [triggerFitView, fitView, setTriggerFitView])

  const onNodeDragStart: OnNodeDrag<Node<EvaNodeData>> = useCallback(() => {
    snapshot()
  }, [snapshot])

  const onNodesChange: OnNodesChange<Node<EvaNodeData>> = useCallback(
    (changes) => applyNodes(changes),
    [applyNodes],
  )

  const onEdgesChange: OnEdgesChange = useCallback(
    (changes) => applyEdges(changes),
    [applyEdges],
  )

  const isValidConnection = useCallback<IsValidConnection>(
    (conn) => {
      const srcMeta = NODE_TYPE_META[nodes.find((n) => n.id === conn.source)?.type ?? '']
      const tgtMeta = NODE_TYPE_META[nodes.find((n) => n.id === conn.target)?.type ?? '']
      const srcPort = srcMeta?.outputs.find((p) => p.name === conn.sourceHandle)
      const tgtPort = tgtMeta?.inputs.find((p) => p.name === conn.targetHandle)
      return srcPort?.category === tgtPort?.category
    },
    [nodes],
  )

  const onConnect = useCallback(
    (conn: Connection) => {
      const srcMeta = NODE_TYPE_META[nodes.find((n) => n.id === conn.source)?.type ?? '']
      const cat = srcMeta?.outputs.find((p) => p.name === conn.sourceHandle)?.category ?? 'data'
      const newEdge: Edge = {
        id: crypto.randomUUID(),
        source: conn.source,
        target: conn.target,
        sourceHandle: conn.sourceHandle ?? null,
        targetHandle: conn.targetHandle ?? null,
        type: cat,
      }
      addEdgeToStore(newEdge)
    },
    [nodes, addEdgeToStore],
  )

  const onDragOver = useCallback(
    (e: React.DragEvent) => {
      if (isOperate) return
      e.preventDefault()
      e.dataTransfer.dropEffect = 'move'
    },
    [isOperate],
  )

  const onDrop = useCallback(
    (e: React.DragEvent) => {
      if (isOperate) return
      e.preventDefault()
      const type = e.dataTransfer.getData('application/eva-node-type')
      if (!type || !NODE_TYPE_META[type]) return
      const position = screenToFlowPosition({ x: e.clientX, y: e.clientY })
      const id = crypto.randomUUID()
      addNodeToStore(buildDefaultNode(id, type, position))
    },
    [isOperate, screenToFlowPosition, addNodeToStore],
  )

  // ---------------------------------------------------------------------------
  // Context menu state
  // ---------------------------------------------------------------------------

  const [contextMenu, setContextMenu] = useState<{
    type: 'pane' | 'node' | 'edge'
    x: number
    y: number
    flowPosition?: { x: number; y: number }
    targetId?: string
  } | null>(null)

  const closeContextMenu = useCallback(() => setContextMenu(null), [])

  const onPaneContextMenu = useCallback(
    (e: React.MouseEvent) => {
      if (isOperate) return
      e.preventDefault()
      const flowPosition = screenToFlowPosition({ x: e.clientX, y: e.clientY })
      setContextMenu({ type: 'pane', x: e.clientX, y: e.clientY, flowPosition })
    },
    [isOperate, screenToFlowPosition],
  )

  const onNodeContextMenu = useCallback(
    (e: React.MouseEvent, node: Node<EvaNodeData>) => {
      if (isOperate) return
      e.preventDefault()
      setContextMenu({ type: 'node', x: e.clientX, y: e.clientY, targetId: node.id })
    },
    [isOperate],
  )

  const onEdgeContextMenu = useCallback(
    (e: React.MouseEvent, edge: Edge) => {
      if (isOperate) return
      e.preventDefault()
      setContextMenu({ type: 'edge', x: e.clientX, y: e.clientY, targetId: edge.id })
    },
    [isOperate],
  )

  const onNodeClick: NodeMouseHandler<Node<EvaNodeData>> = useCallback(
    (_e, node) => { setSelectedNode(node.id); closeContextMenu() },
    [setSelectedNode, closeContextMenu],
  )

  const onEdgeClick: EdgeMouseHandler = useCallback(
    (_e, edge) => { setSelectedEdge(edge.id); closeContextMenu() },
    [setSelectedEdge, closeContextMenu],
  )

  const onPaneClick = useCallback(() => { clearSelection(); closeContextMenu() }, [clearSelection, closeContextMenu])

  // ---------------------------------------------------------------------------
  // Context menu items
  // ---------------------------------------------------------------------------

  const contextMenuItems = useMemo((): MenuItem[] => {
    if (!contextMenu) return []

    if (contextMenu.type === 'pane') {
      const nodeTypeKeys = Object.keys(NODE_TYPE_META) as (keyof typeof NODE_TYPE_META)[]
      return [
        {
          kind: 'submenu',
          label: 'Add Node',
          items: nodeTypeKeys.map((type) => ({
            kind: 'action' as const,
            label: NODE_TYPE_META[type].label,
            icon: NODE_TYPE_META[type].icon,
            onClick: () => {
              if (!contextMenu.flowPosition) return
              const id = crypto.randomUUID()
              addNodeToStore(buildDefaultNode(id, type, contextMenu.flowPosition))
            },
          })),
        },
      ]
    }

    if (contextMenu.type === 'node' && contextMenu.targetId) {
      const targetId = contextMenu.targetId
      return [
        {
          kind: 'action',
          label: 'Duplicate',
          onClick: () => duplicateNodeFromStore(targetId),
        },
        { kind: 'separator' },
        {
          kind: 'action',
          label: 'Delete',
          danger: true,
          onClick: () => { deleteNodeFromStore(targetId); clearSelection() },
        },
      ]
    }

    if (contextMenu.type === 'edge' && contextMenu.targetId) {
      const targetId = contextMenu.targetId
      return [
        {
          kind: 'action',
          label: 'Delete',
          danger: true,
          onClick: () => { deleteEdgeFromStore(targetId); clearSelection() },
        },
      ]
    }

    return []
  }, [contextMenu, addNodeToStore, deleteNodeFromStore, duplicateNodeFromStore, deleteEdgeFromStore, clearSelection])

  // Animate data edges whose source node is currently running
  const animatedEdges = useMemo(
    () =>
      edges.map((e) =>
        e.type === 'data' && nodeStepStates[e.source] === 'running'
          ? { ...e, animated: true }
          : e,
      ),
    [edges, nodeStepStates],
  )

  return (
    <div className="relative flex flex-1 flex-col">
      {selectedProgramId && <KnowledgeStalenessBar programId={selectedProgramId} />}
      <div className={previewOverlayGraph ? 'opacity-30 pointer-events-none flex flex-1 flex-col' : 'flex flex-1 flex-col'}>
        <ReactFlow
          proOptions={{ hideAttribution: true }}
          nodes={nodes}
          edges={animatedEdges}
          nodeTypes={nodeTypes}
          edgeTypes={edgeTypes}
          onNodesChange={isOperate ? undefined : onNodesChange}
          onEdgesChange={isOperate ? undefined : onEdgesChange}
          onConnect={isOperate ? undefined : onConnect}
          onDragOver={onDragOver}
          onDrop={onDrop}
          isValidConnection={isValidConnection}
          onNodeClick={onNodeClick}
          onNodeDragStart={isOperate ? undefined : onNodeDragStart}
          onEdgeClick={onEdgeClick}
          onPaneClick={onPaneClick}
          onPaneContextMenu={onPaneContextMenu}
          onNodeContextMenu={onNodeContextMenu}
          onEdgeContextMenu={onEdgeContextMenu}
          nodesDraggable={!isOperate}
          nodesConnectable={!isOperate}
          edgesReconnectable={!isOperate}
          deleteKeyCode={isOperate ? null : ['Backspace', 'Delete']}
          fitView
          fitViewOptions={{ padding: 0.3 }}
          className="bg-terminal-850 eva-hex-grid"
        >
          <Controls />
          <MiniMap
            nodeColor={(node) => NODE_TYPE_COLORS[node.type ?? 'agent'] ?? MINIMAP_FALLBACK_COLOR}
            maskColor="rgba(10,11,18,0.7)"
          />
        </ReactFlow>

        {/* Empty canvas hint */}
        {nodes.length === 0 && (
          <div className="pointer-events-none absolute inset-0 flex items-center justify-center">
            <div className="flex flex-col items-center gap-2 rounded-md border border-dashed border-terminal-700 px-4 py-3">
              <p className="text-xs text-terminal-400">Drag a Trigger from the palette to start</p>
              <p className="text-xs text-terminal-600">
                or press{' '}
                <kbd className="rounded bg-terminal-700 px-1 py-0.5 font-mono text-terminal-300">⌘K</kbd>{' '}
                to ask MAGI
              </p>
            </div>
          </div>
        )}
      </div>

      {/* Graph preview overlay — shown when the assistant proposes a graph */}
      <GraphPreviewOverlay />

      {contextMenu && (
        <ContextMenu
          x={contextMenu.x}
          y={contextMenu.y}
          items={contextMenuItems}
          onClose={closeContextMenu}
        />
      )}
    </div>
  )
}

export function CanvasContainer() {
  return (
    <ReactFlowProvider>
      <CanvasInner />
    </ReactFlowProvider>
  )
}
