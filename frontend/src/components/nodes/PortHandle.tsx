import { Handle, useNodeConnections, Position } from '@xyflow/react'
import type { HandleType } from '@xyflow/react'
import type { PortDef } from './constants'

interface PortHandleProps {
  port: PortDef
  handleType: HandleType
  topPercent: number
  accentColor: string
}

export function PortHandle({ port, handleType, topPercent, accentColor }: PortHandleProps) {
  const connections = useNodeConnections({ handleType, handleId: port.name })
  const isConnected = connections.length > 0

  const baseStyle =
    'absolute -translate-y-1/2 border-2 transition-colors duration-150'

  const connectedStyle = isConnected ? '' : ''
  const optionalStyle = port.optional ? 'opacity-60' : ''

  const shapeStyle =
    port.category === 'data'
      ? // circle
        'rounded-full w-3 h-3'
      : // diamond: rotate a square
        'rounded-none rotate-45 w-2.5 h-2.5'

  const colorStyle = isConnected
    ? 'border-transparent'
    : 'border-terminal-400 bg-terminal-500'

  return (
    <Handle
      type={handleType}
      position={handleType === 'target' ? Position.Left : Position.Right}
      id={port.name}
      style={{
        top: `${topPercent}%`,
        background: isConnected ? accentColor : undefined,
        borderColor: isConnected ? accentColor : undefined,
        // Override react-flow defaults
        width: port.category === 'data' ? 12 : 10,
        height: port.category === 'data' ? 12 : 10,
        borderRadius: port.category === 'data' ? '50%' : 0,
        transform: `translateY(-50%) ${port.category === 'resource' ? 'rotate(45deg)' : ''}`,
        left: handleType === 'target' ? -6 : undefined,
        right: handleType === 'source' ? -6 : undefined,
      }}
      className={[
        'transition-all duration-150 hover:scale-110 hover:brightness-125',
        isConnected ? '' : 'bg-terminal-500 border-terminal-400',
        optionalStyle,
        connectedStyle,
        baseStyle,
        shapeStyle,
        colorStyle,
      ]
        .filter(Boolean)
        .join(' ')}
      isConnectable
    />
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
