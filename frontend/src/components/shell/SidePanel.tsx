import { useRef } from 'react'
import { BookOpen, History } from 'lucide-react'
import { useUiStore } from '../../store/uiStore'
import { CredentialsPanel } from './CredentialsPanel'
import { ProgramsPanel } from './ProgramsList'
import { NodePalette } from './NodePalette'

const ACTIVITY_LABELS: Record<string, string> = {
  programs: 'Programs',
  nodes: 'Node Palette',
  knowledge: 'Knowledge',
  runs: 'Runs',
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
      <Icon className="h-7 w-7 text-gray-700" />
      <p className="text-xs font-medium text-gray-500">{label}</p>
      <p className="text-center text-[10px] text-gray-600">{note}</p>
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
      className="relative flex flex-shrink-0 flex-col border-r border-gray-800 bg-gray-900"
      style={{ width }}
    >
      {/* Header */}
      <div className="flex h-9 items-center border-b border-gray-800 px-3">
        <span className="text-xs font-semibold uppercase tracking-wider text-gray-400">
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
      {activeActivity === 'runs' && (
        <StubPanel
          icon={History}
          label="Runs"
          note="Execution history and active run indicators. Available in M6."
        />
      )}
      {activeActivity === 'settings' && <CredentialsPanel />}

      {/* Drag handle */}
      <div
        onPointerDown={handleDragStart}
        className="absolute right-0 top-0 h-full w-1 cursor-col-resize hover:bg-blue-500/40 transition-colors"
        aria-hidden
      />
    </div>
  )
}
