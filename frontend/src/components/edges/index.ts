import type { EdgeTypes } from '@xyflow/react'
import { DataEdge } from './DataEdge'
import { ResourceEdge } from './ResourceEdge'

export { DataEdge } from './DataEdge'
export { ResourceEdge } from './ResourceEdge'

/**
 * Passed directly to <ReactFlow edgeTypes={edgeTypes} />.
 * Defined outside any component to prevent re-renders.
 */
export const edgeTypes: EdgeTypes = {
  data: DataEdge,
  resource: ResourceEdge,
}
