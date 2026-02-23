import {
  Brain,
  BookOpen,
  Plug,
  Zap,
  Play,
  Check,
  Loader2,
  Pause,
  AlertCircle,
  type LucideIcon,
} from 'lucide-react'
import type { PortCategory, StepState } from '../types'

// ---------------------------------------------------------------------------
// Port definition (shape of an input/output port on a node)
// ---------------------------------------------------------------------------

export interface PortDef {
  name: string
  label: string
  category: PortCategory
  optional?: boolean
}

// ---------------------------------------------------------------------------
// Node type metadata
// ---------------------------------------------------------------------------

export interface NodeTypeMeta {
  label: string
  icon: LucideIcon
  /** Tailwind bg-* class for the color accent strip */
  accentClass: string
  /** Hex color used for connected handle tint */
  accentColor: string
  inputs: PortDef[]
  outputs: PortDef[]
}

/** Implicit error output port shared by every node type */
const ERROR_PORT: PortDef = { name: 'error', label: 'error', category: 'data' }

export const NODE_TYPE_META: Record<string, NodeTypeMeta> = {
  agent: {
    label: 'Agent',
    icon: Brain,
    accentClass: 'bg-node-agent',
    accentColor: '#7B4AE2',
    inputs: [
      { name: 'instruction', label: 'instruction', category: 'data' },
      { name: 'context', label: 'context', category: 'resource' },
      { name: 'tools', label: 'tools', category: 'resource' },
    ],
    outputs: [
      { name: 'output', label: 'output', category: 'data' },
      ERROR_PORT,
    ],
  },
  knowledge: {
    label: 'Knowledge',
    icon: BookOpen,
    accentClass: 'bg-node-knowledge',
    accentColor: '#00BBFF',
    inputs: [
      { name: 'update', label: 'update', category: 'data', optional: true },
    ],
    outputs: [
      { name: 'content', label: 'content', category: 'resource' },
      ERROR_PORT,
    ],
  },
  connector: {
    label: 'Connector',
    icon: Plug,
    accentClass: 'bg-node-connector',
    accentColor: '#FF8800',
    inputs: [],
    outputs: [
      { name: 'tools', label: 'tools', category: 'resource' },
      ERROR_PORT,
    ],
  },
  action: {
    label: 'Action',
    icon: Zap,
    accentClass: 'bg-node-action',
    accentColor: '#00DD44',
    inputs: [
      { name: 'input', label: 'input', category: 'data' },
      { name: 'tools', label: 'tools', category: 'resource', optional: true },
    ],
    outputs: [
      { name: 'output', label: 'output', category: 'data' },
      ERROR_PORT,
    ],
  },
  trigger: {
    label: 'Trigger',
    icon: Play,
    accentClass: 'bg-node-trigger',
    accentColor: '#FF3B3B',
    inputs: [],
    outputs: [
      { name: 'event', label: 'event', category: 'data' },
      ERROR_PORT,
    ],
  },
}

// ---------------------------------------------------------------------------
// Convenience maps derived from NODE_TYPE_META
// ---------------------------------------------------------------------------

/** Hex accent color per node type */
export const NODE_TYPE_COLORS: Record<string, string> = Object.fromEntries(
  Object.entries(NODE_TYPE_META).map(([k, v]) => [k, v.accentColor]),
)

/** Display label per node type */
export const NODE_TYPE_LABELS: Record<string, string> = Object.fromEntries(
  Object.entries(NODE_TYPE_META).map(([k, v]) => [k, v.label]),
)

/** Lucide icon component per node type */
export const NODE_TYPE_ICONS: Record<string, LucideIcon> = Object.fromEntries(
  Object.entries(NODE_TYPE_META).map(([k, v]) => [k, v.icon]),
)

// ---------------------------------------------------------------------------
// Port type colors — unconnected handle appearance
// ---------------------------------------------------------------------------

export const PORT_TYPE_COLORS: Record<PortCategory, { border: string; bg: string }> = {
  data: { border: 'border-terminal-400', bg: 'bg-terminal-500' },
  resource: { border: 'border-terminal-400', bg: 'bg-terminal-500' },
}

// ---------------------------------------------------------------------------
// Execution state visuals — canvas node rings + badges
// ---------------------------------------------------------------------------

export const STEP_STATE_RING: Record<StepState, string> = {
  completed: 'ring-2 ring-eva-green-500',
  running: 'ring-2 ring-magi-blue-500 animate-glow-pulse',
  waiting: 'ring-2 ring-warn-amber-500',
  failed: 'ring-2 ring-nerv-red-500',
  pending: 'opacity-50',
  skipped: 'opacity-25',
}

export const STEP_STATE_BADGE: Partial<
  Record<StepState, { icon: LucideIcon; className: string }>
> = {
  completed: { icon: Check, className: 'bg-eva-green-500 text-terminal-950' },
  running: { icon: Loader2, className: 'bg-magi-blue-500 text-terminal-950 animate-spin' },
  waiting: { icon: Pause, className: 'bg-warn-amber-500 text-terminal-950' },
  failed: { icon: AlertCircle, className: 'bg-nerv-red-500 text-terminal-50' },
}
