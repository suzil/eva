import { useRef } from 'react'
import { useUiStore } from '../../store/uiStore'

const ACTIVITY_LABELS: Record<string, string> = {
  programs: 'Programs',
  nodes: 'Node Palette',
  knowledge: 'Knowledge',
  runs: 'Runs',
  settings: 'Settings',
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

      {/* Placeholder content */}
      <div className="flex flex-1 items-center justify-center p-4">
        <p className="text-center text-xs text-gray-600">
          {ACTIVITY_LABELS[activeActivity]} panel
          <br />
          (EVA-18 / EVA-21)
        </p>
      </div>

      {/* Drag handle */}
      <div
        onPointerDown={handleDragStart}
        className="absolute right-0 top-0 h-full w-1 cursor-col-resize hover:bg-blue-500/40 transition-colors"
        aria-hidden
      />
    </div>
  )
}
