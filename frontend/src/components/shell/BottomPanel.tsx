import { ChevronDown, ChevronUp } from 'lucide-react'
import { type BottomTab, useUiStore } from '../../store/uiStore'

const TABS: { key: BottomTab; label: string; disabled?: boolean }[] = [
  { key: 'logs', label: 'Logs' },
  { key: 'output', label: 'Output' },
  { key: 'timeline', label: 'Timeline', disabled: true },
]

const BOTTOM_PANEL_HEIGHT = 200

export function BottomPanel() {
  const open = useUiStore((s) => s.bottomPanelOpen)
  const toggle = useUiStore((s) => s.toggleBottomPanel)
  const activeTab = useUiStore((s) => s.activeBottomTab)
  const setActiveTab = useUiStore((s) => s.setActiveBottomTab)

  return (
    <div
      className="flex flex-shrink-0 flex-col border-t border-gray-800 bg-gray-900"
      style={{ height: open ? BOTTOM_PANEL_HEIGHT : undefined }}
    >
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
        <div className="flex flex-1 items-center justify-center overflow-auto">
          <p className="text-xs text-gray-600">
            {activeTab === 'logs' && 'Log output (EVA-28)'}
            {activeTab === 'output' && 'Execution output (EVA-28)'}
          </p>
        </div>
      )}
    </div>
  )
}
