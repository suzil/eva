import { useState, useCallback } from 'react'
import {
  ReactFlow,
  ReactFlowProvider,
  Background,
  Controls,
  MiniMap,
  BackgroundVariant,
  type Node,
  type Edge,
  type OnNodesChange,
  type OnEdgesChange,
  applyNodeChanges,
  applyEdgeChanges,
} from '@xyflow/react'
import '@xyflow/react/dist/style.css'
import { nodeTypes } from '../nodes'
import type { EvaNodeData } from '../../types'

// ---------------------------------------------------------------------------
// Demo nodes — one of each type for visual verification.
// EVA-22 replaces this with Zustand + TanStack Query once graph state
// management is wired up.
// ---------------------------------------------------------------------------
const DEMO_NODES: Node<EvaNodeData>[] = [
  {
    id: 'trigger-1',
    type: 'trigger',
    position: { x: 60, y: 160 },
    data: { label: 'Weekly Trigger', nodeType: { type: 'trigger', config: { type: 'cron', schedule: '0 9 * * 1' } } },
  },
  {
    id: 'knowledge-1',
    type: 'knowledge',
    position: { x: 60, y: 300 },
    data: { label: 'Team Context', nodeType: { type: 'knowledge', config: { source: { type: '_inline_text', value: '' }, format: 'text', refreshPolicy: { type: 'static' } } } },
  },
  {
    id: 'connector-1',
    type: 'connector',
    position: { x: 60, y: 420 },
    data: { label: 'Linear', nodeType: { type: 'connector', config: { system: 'linear', actionFilter: [] } } },
  },
  {
    id: 'agent-1',
    type: 'agent',
    position: { x: 340, y: 200 },
    data: {
      label: 'Summarizer',
      nodeType: {
        type: 'agent',
        config: {
          model: 'gpt-4o',
          systemPrompt: '',
          responseFormat: 'text',
          temperature: 0.7,
          maxIterations: 5,
        },
      },
      // Demo: show a stepState overlay for one node
      stepState: 'running',
    },
  },
  {
    id: 'action-1',
    type: 'action',
    position: { x: 600, y: 220 },
    data: {
      label: 'Format Report',
      nodeType: {
        type: 'action',
        config: {
          operation: 'template',
          parameters: {},
          errorHandling: { mode: 'fail' },
        },
      },
    },
  },
]

const DEMO_EDGES: Edge[] = [
  { id: 'e1', source: 'trigger-1', sourceHandle: 'event', target: 'agent-1', targetHandle: 'instruction' },
  { id: 'e2', source: 'knowledge-1', sourceHandle: 'content', target: 'agent-1', targetHandle: 'context' },
  { id: 'e3', source: 'connector-1', sourceHandle: 'tools', target: 'agent-1', targetHandle: 'tools' },
  { id: 'e4', source: 'agent-1', sourceHandle: 'output', target: 'action-1', targetHandle: 'input' },
]

function CanvasInner() {
  const [nodes, setNodes] = useState<Node<EvaNodeData>[]>(DEMO_NODES)
  const [edges, setEdges] = useState<Edge[]>(DEMO_EDGES)

  const onNodesChange: OnNodesChange<Node<EvaNodeData>> = useCallback(
    (changes) => setNodes((ns) => applyNodeChanges(changes, ns)),
    [],
  )
  const onEdgesChange: OnEdgesChange = useCallback(
    (changes) => setEdges((es) => applyEdgeChanges(changes, es)),
    [],
  )

  return (
    <ReactFlow
      nodes={nodes}
      edges={edges}
      nodeTypes={nodeTypes}
      onNodesChange={onNodesChange}
      onEdgesChange={onEdgesChange}
      fitView
      fitViewOptions={{ padding: 0.3 }}
      className="bg-gray-950"
      deleteKeyCode={null}
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
  )
}

export function CanvasContainer() {
  return (
    <ReactFlowProvider>
      <div className="relative flex flex-1 flex-col">
        <CanvasInner />
      </div>
    </ReactFlowProvider>
  )
}
