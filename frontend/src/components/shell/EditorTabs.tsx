import { type EditorTab, useUiStore } from '../../store/uiStore'
import { ContentArea } from './ContentArea'

const EDITOR_TABS: { key: EditorTab; label: string }[] = [
  { key: 'graph', label: 'Graph' },
  { key: 'code', label: 'Code' },
  { key: 'spec', label: 'Spec' },
]

export function EditorTabs() {
  const activeTab = useUiStore((s) => s.activeEditorTab)
  const setActiveTab = useUiStore((s) => s.setActiveEditorTab)

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Tab bar */}
      <div className="flex h-7 flex-shrink-0 items-center gap-0.5 border-b border-terminal-500 bg-terminal-800 px-2">
        {EDITOR_TABS.map(({ key, label }) => {
          const isActive = activeTab === key
          return (
            <button
              key={key}
              onClick={() => setActiveTab(key)}
              className={[
                'rounded px-3 py-1 font-display text-xs uppercase tracking-widest transition-colors',
                isActive
                  ? 'border-b-2 border-at-field-500 text-terminal-50'
                  : 'text-terminal-300 hover:text-terminal-100',
              ].join(' ')}
            >
              {label}
            </button>
          )
        })}
      </div>

      {/* Content */}
      <div className="flex flex-1 overflow-hidden">
        {activeTab === 'graph' && <ContentArea />}
        {activeTab === 'code' && (
          <div className="flex h-full w-full items-center justify-center">
            <p className="font-display text-xs uppercase tracking-widest text-terminal-500">
              Coming in P2-M4
            </p>
          </div>
        )}
        {activeTab === 'spec' && (
          <div className="flex h-full w-full items-center justify-center">
            <p className="font-display text-xs uppercase tracking-widest text-terminal-500">
              Coming in P2-M3
            </p>
          </div>
        )}
      </div>
    </div>
  )
}
