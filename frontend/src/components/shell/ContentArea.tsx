import { SidePanel } from './SidePanel'
import { CanvasContainer } from './CanvasContainer'
import { DetailPanel } from './DetailPanel'

export function ContentArea() {
  return (
    <div className="flex flex-1 overflow-hidden">
      <SidePanel />
      <CanvasContainer />
      <DetailPanel />
    </div>
  )
}
