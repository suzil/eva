import type { NodeProps } from '@xyflow/react'
import type { Node } from '@xyflow/react'
import type { EvaNodeData } from '../../types'
import { NODE_TYPE_META } from './constants'
import { BaseNode } from './BaseNode'

export type KnowledgeFlowNode = Node<EvaNodeData, 'knowledge'>

export function KnowledgeNode({ id, data, selected }: NodeProps<KnowledgeFlowNode>) {
  const meta = NODE_TYPE_META.knowledge
  return (
    <BaseNode
      id={id}
      label={data.label}
      icon={meta.icon}
      accentClass={meta.accentClass}
      accentColor={meta.accentColor}
      inputs={meta.inputs}
      outputs={meta.outputs}
      stepState={data.stepState}
      selected={selected}
    />
  )
}
