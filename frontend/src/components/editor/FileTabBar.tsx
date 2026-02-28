import { Lock, LockOpen, X } from 'lucide-react'
import type { FileTab } from '../../types'

interface FileTabBarProps {
  tabs: FileTab[]
  activePath: string | null
  isDirty: boolean
  editMode: boolean
  onSelect: (path: string) => void
  onClose: (path: string) => void
  onToggleEditMode: () => void
}

function basename(path: string): string {
  return path.split('/').pop() ?? path
}

export function FileTabBar({
  tabs,
  activePath,
  isDirty,
  editMode,
  onSelect,
  onClose,
  onToggleEditMode,
}: FileTabBarProps) {
  return (
    <div className="flex h-8 flex-shrink-0 items-center border-b border-terminal-500 bg-terminal-800">
      {/* File tabs */}
      <div className="flex min-w-0 flex-1 items-center overflow-x-auto">
        {tabs.map((tab) => {
          const isActive = tab.path === activePath
          const showDirty = isActive && isDirty
          return (
            <button
              key={tab.path}
              onClick={() => onSelect(tab.path)}
              title={tab.path}
              className={[
                'group relative flex h-full flex-shrink-0 items-center gap-1.5 px-3 font-display text-xs uppercase tracking-widest transition-colors',
                isActive
                  ? 'border-b-2 border-at-field-500 text-terminal-50'
                  : 'text-terminal-300 hover:text-terminal-100',
              ].join(' ')}
            >
              {showDirty && (
                <span className="absolute -right-0.5 -top-0.5 h-1.5 w-1.5 rounded-full bg-at-field-500" />
              )}
              <span>{basename(tab.path)}</span>
              <span
                role="button"
                tabIndex={0}
                aria-label={`Close ${basename(tab.path)}`}
                onClick={(e) => {
                  e.stopPropagation()
                  onClose(tab.path)
                }}
                onKeyDown={(e) => {
                  if (e.key === 'Enter' || e.key === ' ') {
                    e.stopPropagation()
                    onClose(tab.path)
                  }
                }}
                className="flex h-4 w-4 items-center justify-center rounded opacity-0 hover:bg-terminal-600 hover:opacity-100 group-hover:opacity-60"
              >
                <X size={10} />
              </span>
            </button>
          )
        })}
      </div>

      {/* Edit mode toggle */}
      <button
        onClick={onToggleEditMode}
        title={editMode ? 'Switch to read-only' : 'Enable editing'}
        className={[
          'flex h-8 w-8 flex-shrink-0 items-center justify-center border-l border-terminal-500 transition-colors',
          editMode
            ? 'text-at-field-400 hover:text-at-field-300'
            : 'text-terminal-400 hover:text-terminal-200',
        ].join(' ')}
      >
        {editMode ? <LockOpen size={14} /> : <Lock size={14} />}
      </button>
    </div>
  )
}
