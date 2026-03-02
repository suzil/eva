import { ChevronRight } from 'lucide-react'
import { useEffect, useRef, type LucideIcon } from 'react'
import ReactDOM from 'react-dom'

export type MenuItem =
  | { kind: 'action'; label: string; icon?: LucideIcon; danger?: boolean; onClick: () => void }
  | { kind: 'submenu'; label: string; icon?: LucideIcon; items: MenuItem[] }
  | { kind: 'separator' }

interface ContextMenuProps {
  x: number
  y: number
  items: MenuItem[]
  onClose: () => void
}

function MenuItems({ items, onClose }: { items: MenuItem[]; onClose: () => void }) {
  return (
    <>
      {items.map((item, i) => {
        if (item.kind === 'separator') {
          return <div key={i} className="my-1 border-t border-terminal-600" />
        }
        if (item.kind === 'submenu') {
          const Icon = item.icon
          return (
            <div key={i}>
              <div className="flex items-center gap-2 px-3 py-1.5 text-sm text-terminal-300 select-none">
                {Icon && <Icon className="h-3.5 w-3.5 shrink-0 text-terminal-400" />}
                <span>{item.label}</span>
                <ChevronRight className="ml-auto h-3 w-3 text-terminal-500" />
              </div>
              <div className="border-t border-terminal-700 pb-1">
                {item.items.map((sub, j) => {
                  if (sub.kind === 'action') {
                    const SubIcon = sub.icon
                    return (
                      <button
                        key={j}
                        onClick={() => {
                          sub.onClick()
                          onClose()
                        }}
                        className={[
                          'flex w-full items-center gap-2 px-6 py-1.5 text-sm',
                          'cursor-pointer text-left',
                          'hover:bg-terminal-700 focus:bg-terminal-700 focus:outline-none',
                          sub.danger ? 'text-red-400' : 'text-terminal-200',
                        ].join(' ')}
                      >
                        {SubIcon && <SubIcon className="h-3.5 w-3.5 shrink-0" />}
                        {sub.label}
                      </button>
                    )
                  }
                  return null
                })}
              </div>
            </div>
          )
        }
        // kind === 'action'
        const Icon = item.icon
        return (
          <button
            key={i}
            onClick={() => {
              item.onClick()
              onClose()
            }}
            className={[
              'flex w-full items-center gap-2 px-3 py-1.5 text-sm',
              'cursor-pointer text-left',
              'hover:bg-terminal-700 focus:bg-terminal-700 focus:outline-none',
              item.danger ? 'text-red-400' : 'text-terminal-200',
            ].join(' ')}
          >
            {Icon && <Icon className="h-3.5 w-3.5 shrink-0" />}
            {item.label}
          </button>
        )
      })}
    </>
  )
}

export function ContextMenu({ x, y, items, onClose }: ContextMenuProps) {
  const menuRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    const handleMouseDown = (e: MouseEvent) => {
      if (menuRef.current && !menuRef.current.contains(e.target as Node)) {
        onClose()
      }
    }
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onClose()
    }
    const handleScroll = () => onClose()

    document.addEventListener('mousedown', handleMouseDown)
    document.addEventListener('keydown', handleKeyDown)
    window.addEventListener('scroll', handleScroll, true)
    return () => {
      document.removeEventListener('mousedown', handleMouseDown)
      document.removeEventListener('keydown', handleKeyDown)
      window.removeEventListener('scroll', handleScroll, true)
    }
  }, [onClose])

  // Clamp to viewport so the menu never renders off-screen
  const vpW = window.innerWidth
  const vpH = window.innerHeight
  const menuW = 192 // min-w-48
  const menuH = 300 // generous estimate; real height unknown until rendered
  const left = Math.min(x, vpW - menuW - 8)
  const top = Math.min(y, vpH - menuH - 8)

  const menu = (
    <div
      ref={menuRef}
      style={{ position: 'fixed', top, left, zIndex: 9999 }}
      className="min-w-48 rounded-md border border-terminal-600 bg-terminal-800 py-1 shadow-lg shadow-black/40"
      onContextMenu={(e) => e.preventDefault()}
    >
      <MenuItems items={items} onClose={onClose} />
    </div>
  )

  return ReactDOM.createPortal(menu, document.body)
}
