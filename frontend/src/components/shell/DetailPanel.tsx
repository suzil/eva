import { useRef } from 'react'
import { useUiStore } from '../../store/uiStore'
import { useCanvasStore } from '../../store/canvasStore'
import { EmptyPanel } from '../detail/EmptyPanel'
import { EdgePanel } from '../detail/EdgePanel'
import { NodePanel } from '../detail/NodePanel'

export function DetailPanel() {
  const width = useUiStore((s) => s.detailPanelWidth)
  const setDetailPanelWidth = useUiStore((s) => s.setDetailPanelWidth)
  const isDragging = useRef(false)

  const selectedNodeId = useCanvasStore((s) => s.selectedNodeId)
  const selectedEdgeId = useCanvasStore((s) => s.selectedEdgeId)

  const handleDragStart = (e: React.PointerEvent) => {
    e.preventDefault()
    isDragging.current = true
    const startX = e.clientX
    const startWidth = width

    const onMove = (ev: PointerEvent) => {
      if (!isDragging.current) return
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
        className="absolute left-0 top-0 h-full w-1 cursor-col-resize transition-colors hover:bg-blue-500/40"
        aria-hidden
      />

      {/* Header */}
      <div className="flex h-9 shrink-0 items-center border-b border-gray-800 px-3">
        <span className="text-xs font-semibold uppercase tracking-wider text-gray-400">
          {selectedNodeId ? 'Node' : selectedEdgeId ? 'Edge' : 'Detail'}
        </span>
      </div>

      {/* Content — adapts to selection */}
      {selectedNodeId ? (
        <NodePanel />
      ) : selectedEdgeId ? (
        <EdgePanel />
      ) : (
        <EmptyPanel />
      )}
    </div>
  )
}
