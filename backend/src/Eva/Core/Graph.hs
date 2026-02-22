{-# LANGUAGE OverloadedStrings #-}

-- | Graph algorithms: topological sort, cycle detection, readiness check.
--
-- Critical rule: topo sort, readiness checks, and terminal/root classification
-- operate on DATA EDGES ONLY. Resource edges are static capability bindings
-- (Knowledge → Agent context, Connector → Agent tools) and do not define
-- execution order in the data flow.
module Eva.Core.Graph
  ( -- * Edge filtering
    dataEdgesOf

    -- * Topological sort / cycle detection
  , topoSort
  , detectCycle

    -- * Node classification
  , rootNodes
  , terminalNodes

    -- * Readiness
  , requiredDataInputs
  , readyNodes
  ) where

import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Eva.Core.Types

-- ---------------------------------------------------------------------------
-- Edge filtering
-- ---------------------------------------------------------------------------

-- | Filter a graph's edges to data edges only.
-- Resource edges are static bindings and do not participate in execution order.
dataEdgesOf :: Graph -> [Edge]
dataEdgesOf = filter (\e -> edgeCategory e == PortData) . graphEdges

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Build the (node, key, [downstream keys]) triples for stronglyConnComp,
-- using only data edges and including every node in the graph (so isolated
-- nodes appear as AcyclicSCC singletons).
buildTriples :: Graph -> [(NodeId, NodeId, [NodeId])]
buildTriples g =
  let nodeIds = Map.keys (graphNodes g)
      edges = dataEdgesOf g
      outgoing :: Map NodeId [NodeId]
      outgoing =
        foldr
          (\e m -> Map.insertWith (++) (edgeSourceNode e) [edgeTargetNode e] m)
          (Map.fromList [(nid, []) | nid <- nodeIds])
          edges
  in [(nid, nid, Map.findWithDefault [] nid outgoing) | nid <- nodeIds]

-- ---------------------------------------------------------------------------
-- Topological sort / cycle detection
-- ---------------------------------------------------------------------------

-- | Topological ordering of all node IDs in the graph, using data edges only.
-- Sources (root nodes) come first.
--
-- Returns @Left cycle@ if the data-edge subgraph contains a cycle, where
-- @cycle@ is the list of node IDs forming the cycle (for error reporting).
-- Returns @Right order@ for a valid DAG.
--
-- Note: 'stronglyConnComp' from containers returns SCCs in reverse
-- topological order (sinks first, via Tarjan's algorithm), so we reverse.
topoSort :: Graph -> Either [NodeId] [NodeId]
topoSort g = fmap reverse (collectSccs (stronglyConnComp (buildTriples g)))

collectSccs :: [SCC NodeId] -> Either [NodeId] [NodeId]
collectSccs [] = Right []
collectSccs (AcyclicSCC nid : rest) = fmap (nid :) (collectSccs rest)
collectSccs (CyclicSCC cyclePath : _) = Left cyclePath

-- | Detect cycles in the data-edge subgraph.
-- Returns @Just cycle@ with the node IDs forming the first cycle found,
-- or @Nothing@ for a valid DAG.
detectCycle :: Graph -> Maybe [NodeId]
detectCycle g =
  case find isCyclic (stronglyConnComp (buildTriples g)) of
    Just (CyclicSCC cyclePath) -> Just cyclePath
    _ -> Nothing
  where
    isCyclic (CyclicSCC _) = True
    isCyclic _ = False

-- ---------------------------------------------------------------------------
-- Node classification
-- ---------------------------------------------------------------------------

-- | Nodes with no incoming data edges.
-- These are the natural starting points for a run. A node may still have
-- incoming resource edges (e.g. a Knowledge node providing context to an
-- Agent) and still be a root in the data flow.
rootNodes :: Graph -> [NodeId]
rootNodes g =
  let nodeIds = Set.fromList (Map.keys (graphNodes g))
      targets = Set.fromList (map edgeTargetNode (dataEdgesOf g))
  in Set.toList (Set.difference nodeIds targets)

-- | Nodes with no outgoing data edges.
-- A run completes when all terminal nodes have finished executing.
terminalNodes :: Graph -> [NodeId]
terminalNodes g =
  let nodeIds = Set.fromList (Map.keys (graphNodes g))
      sources = Set.fromList (map edgeSourceNode (dataEdgesOf g))
  in Set.toList (Set.difference nodeIds sources)

-- ---------------------------------------------------------------------------
-- Readiness
-- ---------------------------------------------------------------------------

-- | Required data input port names for a node type.
-- A node is ready to fire when all of these ports have received a message.
-- Resource ports (context, tools) are not included — they are always
-- available as static bindings and do not gate execution.
requiredDataInputs :: NodeType -> [PortName]
requiredDataInputs (AgentNode _) = ["instruction"]
requiredDataInputs (KnowledgeNode _) = []
requiredDataInputs (ConnectorNode _) = []
requiredDataInputs (ActionNode _) = ["input"]
requiredDataInputs (TriggerNode _) = []

-- | All nodes currently ready to fire, given the set of (node, port) pairs
-- that have a message in their mailbox.
--
-- A node is ready when every port in 'requiredDataInputs' for its type
-- appears in the @filled@ set.
--
-- Nodes with no required data inputs (Knowledge, Connector, Trigger) are
-- always included — they are ready by definition.
readyNodes :: Graph -> Set (NodeId, PortName) -> [NodeId]
readyNodes g filled =
  mapMaybe check (Map.toList (graphNodes g))
  where
    check (nid, node) =
      let required = requiredDataInputs (nodeType node)
      in if all (\port -> Set.member (nid, port) filled) required
           then Just nid
           else Nothing
