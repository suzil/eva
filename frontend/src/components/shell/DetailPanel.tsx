import { useRef } from 'react'
import { useUiStore } from '../../store/uiStore'

export function DetailPanel() {
  const width = useUiStore((s) => s.detailPanelWidth)
  const setDetailPanelWidth = useUiStore((s) => s.setDetailPanelWidth)
  const isDragging = useRef(false)

  const handleDragStart = (e: React.PointerEvent) => {
    e.preventDefault()
    isDragging.current = true
    const startX = e.clientX
    const startWidth = width

    const onMove = (ev: PointerEvent) => {
      if (!isDragging.current) return
      // Moving left increases width; moving right decreases it
      setDetailPanelWidth(startWidth + (startX - ev.clientX))
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
      className="relative flex flex-shrink-0 flex-col border-l border-gray-800 bg-gray-900"
      style={{ width }}
    >
      {/* Drag handle */}
      <div
        onPointerDown={handleDragStart}
        className="absolute left-0 top-0 h-full w-1 cursor-col-resize hover:bg-blue-500/40 transition-colors"
        aria-hidden
      />

      {/* Header */}
      <div className="flex h-9 items-center border-b border-gray-800 px-3">
        <span className="text-xs font-semibold uppercase tracking-wider text-gray-400">
          Detail
        </span>
      </div>

      {/* Placeholder */}
      <div className="flex flex-1 items-center justify-center p-4">
        <p className="text-center text-xs text-gray-600">
          Node configuration
          <br />
          (EVA-21)
        </p>
      </div>
    </div>
  )
}
