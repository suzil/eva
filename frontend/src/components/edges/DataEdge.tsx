import { BaseEdge, getBezierPath, type EdgeProps } from '@xyflow/react'

export function DataEdge({
  sourceX,
  sourceY,
  targetX,
  targetY,
  sourcePosition,
  targetPosition,
  animated,
}: EdgeProps) {
  const [path] = getBezierPath({ sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition })

  return (
    <BaseEdge
      path={path}
      style={{ stroke: '#6366f1', strokeWidth: 1.5 }}
      className={animated ? 'animated' : ''}
    />
  )
}
