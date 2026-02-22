import {
  Check,
  Loader2,
  Pause,
  AlertCircle,
  type LucideIcon,
} from 'lucide-react'
import type { StepState } from '../../types'
import { PortHandle, PortLabel } from './PortHandle'
import type { PortDef } from './constants'

interface BaseNodeProps {
  id: string
  label: string
  subtitle?: string
  icon: LucideIcon
  accentClass: string
  accentColor: string
  inputs: PortDef[]
  outputs: PortDef[]
  stepState?: StepState
  selected?: boolean
}

const STEP_STATE_RING: Record<StepState, string> = {
  completed: 'ring-2 ring-green-500',
  running: 'ring-2 ring-blue-500',
  waiting: 'ring-2 ring-amber-500',
  failed: 'ring-2 ring-red-500',
  pending: 'opacity-50',
  skipped: 'opacity-25',
}

const STEP_STATE_BADGE: Partial<Record<StepState, { icon: LucideIcon; className: string }>> = {
  completed: { icon: Check, className: 'bg-green-500 text-white' },
  running: { icon: Loader2, className: 'bg-blue-500 text-white animate-spin' },
  waiting: { icon: Pause, className: 'bg-amber-500 text-white' },
  failed: { icon: AlertCircle, className: 'bg-red-500 text-white' },
}

/** Compute evenly-spaced top% positions for n ports on one side */
function portTopPercents(count: number): number[] {
  if (count === 0) return []
  return Array.from({ length: count }, (_, i) => ((i + 1) / (count + 1)) * 100)
}

/** Minimum node height in px so the card looks reasonable */
function nodeHeight(inputs: PortDef[], outputs: PortDef[]): number {
  const rows = Math.max(inputs.length, outputs.length, 1)
  return Math.max(64, rows * 28 + 40)
}

export function BaseNode({
  label,
  subtitle,
  icon: Icon,
  accentClass,
  accentColor,
  inputs,
  outputs,
  stepState,
  selected,
}: BaseNodeProps) {
  const height = nodeHeight(inputs, outputs)
  const inputTops = portTopPercents(inputs.length)
  const outputTops = portTopPercents(outputs.length)

  const ringClass = stepState ? STEP_STATE_RING[stepState] : ''
  const badge = stepState ? STEP_STATE_BADGE[stepState] : undefined
  const skippedBorder = stepState === 'skipped' ? 'border-dashed' : 'border-solid'

  const selectionRing = selected && !stepState ? 'ring-2 ring-white/40' : ''

  return (
    <div
      className={[
        'relative flex overflow-visible rounded-lg border border-gray-700 bg-gray-900 shadow-lg',
        skippedBorder,
        ringClass,
        selectionRing,
      ]
        .filter(Boolean)
        .join(' ')}
      style={{ minWidth: 180, height }}
    >
      {/* Left color accent strip */}
      <div className={`w-1 shrink-0 rounded-l-lg ${accentClass}`} />

      {/* Node body */}
      <div className="relative flex flex-1 flex-col px-3 py-2">
        {/* Header: icon + label */}
        <div className="mb-1 flex items-center gap-1.5">
          <Icon
            size={13}
            style={{ color: accentColor }}
            className="shrink-0"
          />
          <span className="truncate text-[11px] font-semibold leading-tight text-gray-200">
            {label}
          </span>
        </div>
        {subtitle && (
          <p className="mb-1 truncate text-[10px] leading-tight text-gray-500">{subtitle}</p>
        )}

        {/* Port labels */}
        {inputs.map((port, i) => (
          <PortLabel key={port.name} port={port} topPercent={inputTops[i]} side="left" />
        ))}
        {outputs.map((port, i) => (
          <PortLabel key={port.name} port={port} topPercent={outputTops[i]} side="right" />
        ))}
      </div>

      {/* Step-state badge (top-right corner) */}
      {badge && (
        <div
          className={[
            'absolute -right-2 -top-2 flex h-4 w-4 items-center justify-center rounded-full',
            badge.className,
          ].join(' ')}
        >
          <badge.icon size={10} />
        </div>
      )}

      {/* Port handles (react-flow) */}
      {inputs.map((port, i) => (
        <PortHandle
          key={port.name}
          port={port}
          handleType="target"
          topPercent={inputTops[i]}
          accentColor={accentColor}
        />
      ))}
      {outputs.map((port, i) => (
        <PortHandle
          key={port.name}
          port={port}
          handleType="source"
          topPercent={outputTops[i]}
          accentColor={accentColor}
        />
      ))}
    </div>
  )
}
