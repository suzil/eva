import { ActivityBar } from './ActivityBar'
import { Toolbar } from './Toolbar'
import { ContentArea } from './ContentArea'
import { BottomPanel } from './BottomPanel'

export function AppShell() {
  return (
    <div className="eva-scanlines flex h-screen overflow-hidden bg-terminal-900 text-terminal-100">
      <ActivityBar />
      <div className="flex flex-1 flex-col overflow-hidden">
        <Toolbar />
        <ContentArea />
        <BottomPanel />
      </div>
    </div>
  )
}
