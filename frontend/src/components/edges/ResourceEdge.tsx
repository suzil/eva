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
      style={{ stroke: 'var(--terminal-400)', strokeWidth: 1.5, strokeDasharray: '5 4' }}
    />
  )
}
