import { useRef } from 'react'
import { BookOpen } from 'lucide-react'
import { useUiStore } from '../../store/uiStore'
import { CredentialsPanel } from './CredentialsPanel'
import { ProgramsPanel } from './ProgramsList'
import { NodePalette } from './NodePalette'
import { RunsPanel } from './RunsPanel'
import { CodebasePanel } from '../panels/CodebasePanel'

const ACTIVITY_LABELS: Record<string, string> = {
  programs: 'Programs',
  nodes: 'Node Palette',
  knowledge: 'Knowledge',
  runs: 'Runs',
  codebase: 'Codebase',
  settings: 'Settings',
}

function StubPanel({
  icon: Icon,
  label,
  note,
}: {
  icon: React.ComponentType<{ className?: string }>
  label: string
  note: string
}) {
  return (
    <div className="flex flex-1 flex-col items-center justify-center gap-2 p-4">
      <Icon className="h-7 w-7 text-terminal-600" />
      <p className="text-xs font-medium text-terminal-400">{label}</p>
      <p className="text-center text-[10px] text-terminal-500">{note}</p>
    </div>
  )
}

export function SidePanel() {
  const width = useUiStore((s) => s.sidePanelWidth)
  const setSidePanelWidth = useUiStore((s) => s.setSidePanelWidth)
  const activeActivity = useUiStore((s) => s.activeActivity)
  const isDragging = useRef(false)

  const handleDragStart = (e: React.PointerEvent) => {
    e.preventDefault()
    isDragging.current = true
    const startX = e.clientX
    const startWidth = width

    const onMove = (ev: PointerEvent) => {
      if (!isDragging.current) return
      setSidePanelWidth(startWidth + (ev.clientX - startX))
    }

    const onUp = () => {
      isDragging.current = false
      document.removeEventListener('pointermove', onMove)
      document.removeEventListener('pointerup', onUp)
    }

    document.addEventListener('pointermove', onMove)
    document.addEventListener('pointerup', onUp)
  }

  return (
    <div
      className="relative flex flex-shrink-0 flex-col border-r border-terminal-500 bg-terminal-800"
      style={{ width }}
    >
      {/* Header */}
      <div className="flex h-9 items-center border-b border-terminal-500 px-3">
        <span className="font-display text-xs uppercase tracking-widest text-terminal-300">
          {ACTIVITY_LABELS[activeActivity]}
        </span>
      </div>

      {/* Panel content — routed by active activity */}
      {activeActivity === 'programs' && <ProgramsPanel />}
      {activeActivity === 'nodes' && <NodePalette />}
      {activeActivity === 'knowledge' && (
        <StubPanel
          icon={BookOpen}
          label="Knowledge Library"
          note="Shared knowledge sources across programs. Available in M5."
        />
      )}
      {activeActivity === 'runs' && <RunsPanel />}
      {activeActivity === 'codebase' && <CodebasePanel />}
      {activeActivity === 'settings' && <CredentialsPanel />}

      {/* Drag handle */}
      <div
        onPointerDown={handleDragStart}
        className="absolute right-0 top-0 h-full w-1 cursor-col-resize hover:bg-at-field-500/30 transition-colors"
        aria-hidden
      />
    </div>
  )
}
