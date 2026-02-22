import type { NodeProps } from '@xyflow/react'
import type { Node } from '@xyflow/react'
import cronstrue from 'cronstrue'
import type { EvaNodeData } from '../../types'
import { NODE_TYPE_META } from './constants'
import { BaseNode } from './BaseNode'

export type TriggerFlowNode = Node<EvaNodeData, 'trigger'>

function cronSubtitle(schedule: string): string | undefined {
  try {
    return cronstrue.toString(schedule, { verbose: false, throwExceptionOnParseError: true })
  } catch {
    return undefined
  }
}

export function TriggerNode({ id, data, selected }: NodeProps<TriggerFlowNode>) {
  const meta = NODE_TYPE_META.trigger

  const subtitle =
    data.nodeType.type === 'trigger' && data.nodeType.config.type === 'cron'
      ? cronSubtitle(data.nodeType.config.schedule ?? '')
      : undefined

  return (
    <BaseNode
      id={id}
      label={data.label}
      subtitle={subtitle}
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
