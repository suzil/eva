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
      style={{
        stroke: animated ? 'var(--at-field-500)' : 'var(--terminal-400)',
        strokeWidth: 1.5,
        ...(animated ? { strokeDasharray: '8 4' } : {}),
      }}
      className={animated ? 'animate-dash-flow' : ''}
    />
  )
}
