import { type EditorTab, useUiStore } from '../../store/uiStore'
import { ContentArea } from './ContentArea'
import { CodeEditorView } from '../editor/CodeEditorView'
import { SpecEditorView } from '../editor/SpecEditorView'

const EDITOR_TABS: { key: EditorTab; label: string }[] = [
  { key: 'graph', label: 'Graph' },
  { key: 'code', label: 'Code' },
  { key: 'spec', label: 'Spec' },
]

export function EditorTabs() {
  const activeTab = useUiStore((s) => s.activeEditorTab)
  const setActiveTab = useUiStore((s) => s.setActiveEditorTab)
  const specDirty = useUiStore((s) => s.specDirty)
  const specSyncState = useUiStore((s) => s.specSyncState)
  const setSpecSyncState = useUiStore((s) => s.setSpecSyncState)

  const handleTabChange = (newTab: EditorTab) => {
    // T2: leaving SPEC with dirty YAML — note conflict so modal appears when user returns
    if (activeTab === 'spec' && newTab !== 'spec' && specDirty) {
      setSpecSyncState('conflict')
    }
    // T1: entering SPEC fresh (not returning to an already-flagged conflict) — mark YAML as source
    if (newTab === 'spec' && specSyncState !== 'conflict') {
      setSpecSyncState('yaml_source')
    }
    setActiveTab(newTab)
  }

  return (
    <div className="flex flex-1 flex-col overflow-hidden">
      {/* Tab bar */}
      <div className="flex h-7 flex-shrink-0 items-center gap-0.5 border-b border-terminal-500 bg-terminal-800 px-2">
        {EDITOR_TABS.map(({ key, label }) => {
          const isActive = activeTab === key
          const showDirtyDot = key === 'spec' && specDirty
          return (
            <button
              key={key}
              onClick={() => handleTabChange(key)}
              className={[
                'relative rounded px-3 py-1 font-display text-xs uppercase tracking-widest transition-colors',
                isActive
                  ? 'border-b-2 border-at-field-500 text-terminal-50'
                  : 'text-terminal-300 hover:text-terminal-100',
              ].join(' ')}
            >
              {label}
              {showDirtyDot && (
                <span className="absolute -right-0.5 -top-0.5 h-1.5 w-1.5 rounded-full bg-at-field-500" />
              )}
            </button>
          )
        })}
      </div>

      {/* Content */}
      <div className="flex flex-1 overflow-hidden">
        {activeTab === 'graph' && <ContentArea />}
        {activeTab === 'code' && <CodeEditorView />}
        {activeTab === 'spec' && <SpecEditorView />}
      </div>
    </div>
  )
}
