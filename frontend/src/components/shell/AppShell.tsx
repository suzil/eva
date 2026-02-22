import { ActivityBar } from './ActivityBar'
import { Toolbar } from './Toolbar'
import { ContentArea } from './ContentArea'
import { BottomPanel } from './BottomPanel'

export function AppShell() {
  return (
    <div className="flex h-screen overflow-hidden bg-gray-950 text-white">
      <ActivityBar />
      <div className="flex flex-1 flex-col overflow-hidden">
        <Toolbar />
        <ContentArea />
        <BottomPanel />
      </div>
    </div>
  )
}
