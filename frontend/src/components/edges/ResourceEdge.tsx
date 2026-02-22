import { BaseEdge, getBezierPath, type EdgeProps } from '@xyflow/react'

export function ResourceEdge({
  sourceX,
  sourceY,
  targetX,
  targetY,
  sourcePosition,
  targetPosition,
}: EdgeProps) {
  const [path] = getBezierPath({ sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition })

  return (
    <BaseEdge
      path={path}
      style={{ stroke: '#6b7280', strokeWidth: 1.5, strokeDasharray: '5 4' }}
    />
  )
}
