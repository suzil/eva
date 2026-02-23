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
      className="relative flex flex-shrink-0 flex-col bg-terminal-800"
      style={{ height: open ? height : undefined }}
    >
      {/* Drag handle — sits at the very top edge, only shown when open */}
      {open && (
        <div
          onPointerDown={handleDragStart}
          className="absolute top-0 left-0 right-0 h-1 cursor-row-resize hover:bg-at-field-500/40 transition-colors z-10"
          aria-hidden
        />
      )}

      {/* Top border */}
      <div className="h-px flex-shrink-0 bg-terminal-500" />

      {/* Tab bar */}
      <div className="flex h-9 flex-shrink-0 items-center border-b border-terminal-500 px-2">
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
                    ? 'cursor-not-allowed text-terminal-500'
                    : isActive
                    ? 'border-b-2 border-at-field-500 text-terminal-50'
                    : 'text-terminal-300 hover:bg-terminal-700 hover:text-terminal-100',
                ].join(' ')}
                title={disabled ? 'Available in M7' : undefined}
              >
                {label}
                {disabled && (
                  <span className="ml-1 text-terminal-500 text-[10px]">—</span>
                )}
              </button>
            )
          })}
        </div>

        <button
          onClick={toggle}
          className="ml-auto rounded p-1 text-terminal-400 hover:bg-terminal-700 hover:text-terminal-100 transition-colors"
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
