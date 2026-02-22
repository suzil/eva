import type { NodeProps } from '@xyflow/react'
import type { Node } from '@xyflow/react'
import type { EvaNodeData } from '../../types'
import { NODE_TYPE_META } from './constants'
import { BaseNode } from './BaseNode'

export type ActionFlowNode = Node<EvaNodeData, 'action'>

export function ActionNode({ id, data, selected }: NodeProps<ActionFlowNode>) {
  const meta = NODE_TYPE_META.action
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
