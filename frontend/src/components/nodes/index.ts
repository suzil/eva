import type { NodeTypes } from '@xyflow/react'
import { AgentNode } from './AgentNode'
import { KnowledgeNode } from './KnowledgeNode'
import { ConnectorNode } from './ConnectorNode'
import { ActionNode } from './ActionNode'
import { TriggerNode } from './TriggerNode'

export { AgentNode } from './AgentNode'
export { KnowledgeNode } from './KnowledgeNode'
export { ConnectorNode } from './ConnectorNode'
export { ActionNode } from './ActionNode'
export { TriggerNode } from './TriggerNode'
export { NODE_TYPE_META } from './constants'
export type { PortDef, NodeTypeMeta } from './constants'

/**
 * Passed directly to <ReactFlow nodeTypes={nodeTypes} />.
 * Defined outside any component to prevent re-renders.
 */
export const nodeTypes: NodeTypes = {
  agent: AgentNode,
  knowledge: KnowledgeNode,
  connector: ConnectorNode,
  action: ActionNode,
  trigger: TriggerNode,
}
