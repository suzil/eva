import type { NodeProps } from '@xyflow/react'
import type { Node } from '@xyflow/react'
import type { EvaNodeData } from '../../types'
import { NODE_TYPE_META } from './constants'
import { BaseNode } from './BaseNode'

export type ConnectorFlowNode = Node<EvaNodeData, 'connector'>

export function ConnectorNode({ id, data, selected }: NodeProps<ConnectorFlowNode>) {
  const meta = NODE_TYPE_META.connector
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
