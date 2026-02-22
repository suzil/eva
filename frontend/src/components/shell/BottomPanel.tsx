import { useRef } from 'react'
import { ChevronDown, ChevronUp } from 'lucide-react'
import { type BottomTab, useUiStore } from '../../store/uiStore'
import { LogsPanel } from './LogsPanel'
import { OutputPanel } from './OutputPanel'

const TABS: { key: BottomTab; label: string; disabled?: boolean }[] = [
  { key: 'logs', label: 'Logs' },
  { key: 'output', label: 'Output' },
  { key: 'timeline', label: 'Timeline', disabled: true },
]

export function BottomPanel() {
  const open = useUiStore((s) => s.bottomPanelOpen)
  const toggle = useUiStore((s) => s.toggleBottomPanel)
  const activeTab = useUiStore((s) => s.activeBottomTab)
  const setActiveTab = useUiStore((s) => s.setActiveBottomTab)
  const height = useUiStore((s) => s.bottomPanelHeight)
  const setHeight = useUiStore((s) => s.setBottomPanelHeight)
  const isDragging = useRef(false)

  const handleDragStart = (e: React.PointerEvent) => {
    e.preventDefault()
    isDragging.current = true
    const startY = e.clientY
    const startHeight = height

    const onMove = (ev: PointerEvent) => {
      if (!isDragging.current) return
      // Dragging up (decreasing Y) should increase the panel height
      setHeight(startHeight + (startY - ev.clientY))
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
      className="relative flex flex-shrink-0 flex-col bg-gray-900"
      style={{ height: open ? height : undefined }}
    >
      {/* Drag handle — sits at the very top edge, only shown when open */}
      {open && (
        <div
          onPointerDown={handleDragStart}
          className="absolute top-0 left-0 right-0 h-1 cursor-row-resize hover:bg-blue-500/40 transition-colors z-10"
          aria-hidden
        />
      )}

      {/* Top border */}
      <div className="h-px flex-shrink-0 bg-gray-800" />

      {/* Tab bar */}
      <div className="flex h-9 flex-shrink-0 items-center border-b border-gray-800 px-2">
        <div className="flex flex-1 items-center gap-0.5">
          {TABS.map(({ key, label, disabled }) => {
            const isActive = activeTab === key && open
            return (
              <button
                key={key}
                onClick={() => {
                  if (disabled) return
                  if (!open) toggle()
                  setActiveTab(key)
                }}
                disabled={disabled}
                className={[
                  'rounded px-3 py-1 text-xs font-medium transition-colors',
                  disabled
                    ? 'cursor-not-allowed text-gray-700'
                    : isActive
                    ? 'bg-gray-700 text-white'
                    : 'text-gray-400 hover:bg-gray-800 hover:text-white',
                ].join(' ')}
                title={disabled ? 'Available in M7' : undefined}
              >
                {label}
                {disabled && (
                  <span className="ml-1 text-gray-700 text-[10px]">—</span>
                )}
              </button>
            )
          })}
        </div>

        <button
          onClick={toggle}
          className="ml-auto rounded p-1 text-gray-500 hover:bg-gray-800 hover:text-white transition-colors"
          aria-label={open ? 'Collapse bottom panel' : 'Expand bottom panel'}
          title={open ? 'Collapse' : 'Expand'}
        >
          {open ? (
            <ChevronDown className="h-4 w-4" />
          ) : (
            <ChevronUp className="h-4 w-4" />
          )}
        </button>
      </div>

      {/* Content */}
      {open && (
        <div className="flex flex-1 overflow-hidden">
          {activeTab === 'logs' && <LogsPanel />}
          {activeTab === 'output' && <OutputPanel />}
        </div>
      )}
    </div>
  )
}
