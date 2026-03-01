import { useState } from 'react'
import { Handle, useNodeConnections, Position } from '@xyflow/react'
import type { HandleType } from '@xyflow/react'
import type { PortDef } from './constants'
import { PORT_TYPE_COLORS } from '../../constants/nodeConstants'

interface PortHandleProps {
  port: PortDef
  handleType: HandleType
  topPercent: number
  accentColor: string
}

export function PortHandle({ port, handleType, topPercent, accentColor }: PortHandleProps) {
  const [hovered, setHovered] = useState(false)
  const connections = useNodeConnections({ handleType, handleId: port.name })
  const isConnected = connections.length > 0

  const baseStyle =
    'absolute -translate-y-1/2 border-2 transition-colors duration-150'

  const optionalStyle = port.optional ? 'opacity-60' : ''

  const shapeStyle =
    port.category === 'data'
      ? // circle — 16px for ≥16px hit area
        'rounded-full w-4 h-4'
      : // diamond: rotate a square — 12×12 gives ~17px diagonal hit area
        'rounded-none rotate-45 w-3 h-3'

  const { border, bg } = PORT_TYPE_COLORS[port.category]
  const colorStyle = isConnected ? 'border-transparent' : `${border} ${bg}`

  // Tooltip positioned just outside the node card edge
  const tooltipStyle: React.CSSProperties = {
    top: `${topPercent}%`,
    ...(handleType === 'target'
      ? { left: 0, transform: 'translateX(calc(-100% - 8px)) translateY(-50%)' }
      : { right: 0, transform: 'translateX(calc(100% + 8px)) translateY(-50%)' }),
  }

  return (
    <>
      <Handle
        type={handleType}
        position={handleType === 'target' ? Position.Left : Position.Right}
        id={port.name}
        style={{
          top: `${topPercent}%`,
          background: isConnected ? accentColor : undefined,
          borderColor: isConnected ? accentColor : undefined,
          // Override react-flow defaults
          width: port.category === 'data' ? 16 : 12,
          height: port.category === 'data' ? 16 : 12,
          borderRadius: port.category === 'data' ? '50%' : 0,
          transform: `translateY(-50%) ${port.category === 'resource' ? 'rotate(45deg)' : ''}`,
          left: handleType === 'target' ? -8 : undefined,
          right: handleType === 'source' ? -8 : undefined,
        }}
        className={[
          'transition-all duration-150 hover:scale-110 hover:brightness-125',
          optionalStyle,
          baseStyle,
          shapeStyle,
          colorStyle,
        ]
          .filter(Boolean)
          .join(' ')}
        isConnectable
        onMouseEnter={() => setHovered(true)}
        onMouseLeave={() => setHovered(false)}
      />
      {hovered && (
        <div
          className="pointer-events-none absolute z-[100] flex items-center gap-1 rounded border border-terminal-500 bg-terminal-700 px-1.5 py-0.5 text-[10px] whitespace-nowrap shadow-md"
          style={tooltipStyle}
        >
          <span className="text-terminal-400">
            {port.category === 'data' ? '○' : '◇'}
          </span>
          <span className="text-terminal-100">{port.label}</span>
          <span className="text-terminal-400">{port.category}</span>
        </div>
      )}
    </>
  )
}

/** Vertical port label rendered inside the node body, aligned to the handle row */
interface PortLabelProps {
  port: PortDef
  topPercent: number
  side: 'left' | 'right'
}

export function PortLabel({ port, topPercent, side }: PortLabelProps) {
  return (
    <div
      className={[
        'pointer-events-none absolute text-[10px] leading-none text-terminal-300 select-none',
        side === 'left' ? 'left-4' : 'right-4',
        side === 'right' ? 'text-right' : 'text-left',
      ].join(' ')}
      style={{ top: `${topPercent}%`, transform: 'translateY(-50%)' }}
    >
      {port.label}
    </div>
  )
}
