import { Brain, BookOpen, Plug, Zap, Play, type LucideIcon } from 'lucide-react'
import type { PortCategory } from '../../types'

export interface PortDef {
  name: string
  label: string
  category: PortCategory
  optional?: boolean
}

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
    accentClass: 'bg-indigo-500',
    accentColor: '#6366f1',
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
    accentClass: 'bg-amber-500',
    accentColor: '#f59e0b',
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
    accentClass: 'bg-purple-500',
    accentColor: '#a855f7',
    inputs: [],
    outputs: [
      { name: 'tools', label: 'tools', category: 'resource' },
      ERROR_PORT,
    ],
  },
  action: {
    label: 'Action',
    icon: Zap,
    accentClass: 'bg-emerald-500',
    accentColor: '#10b981',
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
    accentClass: 'bg-red-500',
    accentColor: '#ef4444',
    inputs: [],
    outputs: [
      { name: 'event', label: 'event', category: 'data' },
      ERROR_PORT,
    ],
  },
}
