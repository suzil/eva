import { LayoutList, Boxes, BookOpen, History, FolderCode, Settings } from 'lucide-react'
import { type ActivityKey, useUiStore } from '../../store/uiStore'

interface ActivityItem {
  key: ActivityKey
  label: string
  Icon: React.ComponentType<{ className?: string }>
}

const ACTIVITIES: ActivityItem[] = [
  { key: 'programs', label: 'Programs', Icon: LayoutList },
  { key: 'nodes', label: 'Node Palette', Icon: Boxes },
  { key: 'knowledge', label: 'Knowledge', Icon: BookOpen },
  { key: 'runs', label: 'Runs', Icon: History },
  { key: 'codebase', label: 'Codebase', Icon: FolderCode },
]

const BOTTOM_ACTIVITIES: ActivityItem[] = [
  { key: 'settings', label: 'Settings', Icon: Settings },
]

export function ActivityBar() {
  const activeActivity = useUiStore((s) => s.activeActivity)
  const setActiveActivity = useUiStore((s) => s.setActiveActivity)

  const renderItem = ({ key, label, Icon }: ActivityItem) => {
    const isActive = activeActivity === key
    return (
      <button
        key={key}
        title={label}
        onClick={() => setActiveActivity(key)}
        className={[
          'group relative flex h-12 w-12 items-center justify-center rounded-md transition-colors',
          isActive
            ? 'text-terminal-50'
            : 'text-terminal-400 hover:bg-terminal-600 hover:text-terminal-100',
        ].join(' ')}
        aria-label={label}
        aria-pressed={isActive}
      >
        <Icon className="h-5 w-5" />
        {isActive && (
          <span className="absolute left-0 top-1/2 h-5 w-0.5 -translate-y-1/2 rounded-r bg-at-field-500" />
        )}
      </button>
    )
  }

  return (
    <aside className="flex w-12 flex-shrink-0 flex-col items-center gap-1 bg-terminal-900 py-2 border-r border-terminal-500">
      <div className="flex flex-1 flex-col items-center gap-1">
        {ACTIVITIES.map(renderItem)}
      </div>
      <div className="flex flex-col items-center gap-1 pb-1">
        {BOTTOM_ACTIVITIES.map(renderItem)}
      </div>
    </aside>
  )
}
