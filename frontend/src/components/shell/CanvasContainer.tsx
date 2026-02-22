import { useCallback, useEffect, useMemo } from 'react'
import {
  ReactFlow,
  ReactFlowProvider,
  Background,
  Controls,
  MiniMap,
  BackgroundVariant,
  useReactFlow,
  type Node,
  type Edge,
  type OnNodesChange,
  type OnEdgesChange,
  type Connection,
  type IsValidConnection,
  type NodeMouseHandler,
  type EdgeMouseHandler,
  type NodeDragHandler,
} from '@xyflow/react'
import '@xyflow/react/dist/style.css'
import { nodeTypes } from '../nodes'
import { NODE_TYPE_META } from '../nodes/constants'
import { edgeTypes } from '../edges'
import type { EvaNodeData, NodeType } from '../../types'
import { useCanvasStore } from '../../store/canvasStore'
import { useUiStore } from '../../store/uiStore'
import { useProgram } from '../../api/hooks'

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
  const loadGraph = useCanvasStore((s) => s.loadGraph)
  const applyNodes = useCanvasStore((s) => s.applyNodeChanges)
  const applyEdges = useCanvasStore((s) => s.applyEdgeChanges)
  const addEdgeToStore = useCanvasStore((s) => s.addEdge)
  const addNodeToStore = useCanvasStore((s) => s.addNode)
  const setSelectedNode = useCanvasStore((s) => s.setSelectedNode)
  const setSelectedEdge = useCanvasStore((s) => s.setSelectedEdge)
  const clearSelection = useCanvasStore((s) => s.clearSelection)
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

  const onNodeDragStart: NodeDragHandler = useCallback(() => {
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

  const onNodeClick: NodeMouseHandler<Node<EvaNodeData>> = useCallback(
    (_e, node) => setSelectedNode(node.id),
    [setSelectedNode],
  )

  const onEdgeClick: EdgeMouseHandler = useCallback(
    (_e, edge) => setSelectedEdge(edge.id),
    [setSelectedEdge],
  )

  const onPaneClick = useCallback(() => clearSelection(), [clearSelection])

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
      <ReactFlow
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
        nodesDraggable={!isOperate}
        nodesConnectable={!isOperate}
        edgesUpdatable={!isOperate}
        deleteKeyCode={isOperate ? null : ['Backspace', 'Delete']}
        fitView
        fitViewOptions={{ padding: 0.3 }}
        className="bg-gray-950"
      >
        <Background
          variant={BackgroundVariant.Dots}
          gap={24}
          size={1}
          color="#374151"
        />
        <Controls
          className="[&>button]:border-gray-700 [&>button]:bg-gray-800 [&>button]:text-gray-300 [&>button:hover]:bg-gray-700"
        />
        <MiniMap
          className="!bg-gray-900"
          nodeColor={(node) => {
            const typeKey = node.type ?? 'agent'
            const colors: Record<string, string> = {
              agent: '#6366f1',
              knowledge: '#f59e0b',
              connector: '#a855f7',
              action: '#10b981',
              trigger: '#ef4444',
            }
            return colors[typeKey] ?? '#6b7280'
          }}
          maskColor="rgba(17,24,39,0.7)"
        />
      </ReactFlow>

      {/* Empty canvas hint */}
      {nodes.length === 0 && (
        <div className="pointer-events-none absolute inset-0 flex items-center justify-center">
          <p className="rounded-md border border-dashed border-gray-700 px-4 py-2 text-xs text-gray-500">
            Drag a Trigger from the palette to start
          </p>
        </div>
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
