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
