{-# LANGUAGE OverloadedStrings #-}

-- | Full semantic validation of a Graph.
-- Checks port compatibility, required wiring, config completeness,
-- cycle detection, and reachability. Used by the /validate endpoint.
module Eva.Core.Validation
  ( validateGraph
  ) where

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Eva.Core.Graph (dataEdgesOf, detectCycle)
import Eva.Core.Types

-- ---------------------------------------------------------------------------
-- Port spec tables (from eva-concepts.mdc)
-- ---------------------------------------------------------------------------

inputPorts :: NodeType -> [PortSpec]
inputPorts (AgentNode _) =
  [ PortSpec "instruction" PortData     False
  , PortSpec "context"     PortResource True
  , PortSpec "tools"       PortResource True
  ]
inputPorts (KnowledgeNode _) =
  [ PortSpec "update" PortData True
  ]
inputPorts (ConnectorNode _) = []
inputPorts (ActionNode _) =
  [ PortSpec "input" PortData     False
  , PortSpec "tools" PortResource True
  ]
inputPorts (TriggerNode _) = []

outputPorts :: NodeType -> [PortSpec]
outputPorts (AgentNode _)     = [PortSpec "output" PortData False]
outputPorts (KnowledgeNode _) = [PortSpec "content" PortResource False]
outputPorts (ConnectorNode _) = [PortSpec "tools"   PortResource False]
outputPorts (ActionNode _)    = [PortSpec "output"  PortData     False]
outputPorts (TriggerNode _)   = [PortSpec "event"   PortData     False]

-- ---------------------------------------------------------------------------
-- Main entry point
-- ---------------------------------------------------------------------------

-- | Run all validation checks on a graph, returning accumulated errors.
-- Checks are ordered: structural checks first, semantic checks after.
validateGraph :: Graph -> [ValidationError]
validateGraph g =
  checkTriggerPresence g
    <> checkDag g
    <> checkEdgePortNames g
    <> checkPortCategories g
    <> checkRequiredWiring g
    <> checkConfigComplete g
    <> checkReachability g

-- ---------------------------------------------------------------------------
-- Check 1: Trigger presence
-- ---------------------------------------------------------------------------

checkTriggerPresence :: Graph -> [ValidationError]
checkTriggerPresence g =
  let triggers = filter isTrigger (Map.elems (graphNodes g))
  in if null triggers
       then [ValidationError { veMessage = "Graph must contain at least one Trigger node", veNodeId = Nothing }]
       else []
  where
    isTrigger n = case nodeType n of
      TriggerNode _ -> True
      _             -> False

-- ---------------------------------------------------------------------------
-- Check 2: DAG (no cycles in data edges)
-- ---------------------------------------------------------------------------

checkDag :: Graph -> [ValidationError]
checkDag g =
  case detectCycle g of
    Nothing    -> []
    Just cyclePath ->
      [ ValidationError
          { veMessage =
              "Cycle detected in data edges among nodes: "
                <> T.intercalate ", " (map unNodeId cyclePath)
          , veNodeId = Nothing
          }
      ]

-- ---------------------------------------------------------------------------
-- Check 3 & 4: Edge port names valid on source and target node types
-- ---------------------------------------------------------------------------

checkEdgePortNames :: Graph -> [ValidationError]
checkEdgePortNames g =
  concatMap checkEdge (graphEdges g)
  where
    nodes :: Map NodeId Node
    nodes = graphNodes g

    checkEdge :: Edge -> [ValidationError]
    checkEdge e =
      srcErrors e <> tgtErrors e

    srcErrors :: Edge -> [ValidationError]
    srcErrors e =
      case Map.lookup (edgeSourceNode e) nodes of
        Nothing -> []  -- dangling edge; handled by structural check elsewhere
        Just srcNode ->
          let declared = map portSpecName (outputPorts (nodeType srcNode))
          in if edgeSourcePort e `elem` declared
               then []
               else
                 [ ValidationError
                     { veMessage =
                         "Edge source port '"
                           <> unPortName (edgeSourcePort e)
                           <> "' is not a valid output port of node '"
                           <> nodeLabel srcNode
                           <> "' ("
                           <> nodeTypeName (nodeType srcNode)
                           <> "). Valid output ports: "
                           <> T.intercalate ", " (map unPortName declared)
                     , veNodeId = Nothing
                     }
                 ]

    tgtErrors :: Edge -> [ValidationError]
    tgtErrors e =
      case Map.lookup (edgeTargetNode e) nodes of
        Nothing -> []
        Just tgtNode ->
          let declared = map portSpecName (inputPorts (nodeType tgtNode))
          in if edgeTargetPort e `elem` declared
               then []
               else
                 [ ValidationError
                     { veMessage =
                         "Edge target port '"
                           <> unPortName (edgeTargetPort e)
                           <> "' is not a valid input port of node '"
                           <> nodeLabel tgtNode
                           <> "' ("
                           <> nodeTypeName (nodeType tgtNode)
                           <> "). Valid input ports: "
                           <> T.intercalate ", " (map unPortName declared)
                     , veNodeId = Nothing
                     }
                 ]

-- ---------------------------------------------------------------------------
-- Check 5: Port category compatibility
-- ---------------------------------------------------------------------------

checkPortCategories :: Graph -> [ValidationError]
checkPortCategories g =
  concatMap checkEdge (graphEdges g)
  where
    nodes :: Map NodeId Node
    nodes = graphNodes g

    checkEdge :: Edge -> [ValidationError]
    checkEdge e = srcCatErrors e <> tgtCatErrors e

    portCategory :: [PortSpec] -> PortName -> Maybe PortCategory
    portCategory specs name =
      fmap portSpecCategory (find (\s -> portSpecName s == name) specs)

    srcCatErrors :: Edge -> [ValidationError]
    srcCatErrors e =
      case Map.lookup (edgeSourceNode e) nodes of
        Nothing -> []
        Just srcNode ->
          case portCategory (outputPorts (nodeType srcNode)) (edgeSourcePort e) of
            Nothing -> []  -- already caught by port name check
            Just declared ->
              if declared == edgeCategory e
                then []
                else
                  [ ValidationError
                      { veMessage =
                          "Edge category mismatch on source port '"
                            <> unPortName (edgeSourcePort e)
                            <> "' of node '"
                            <> nodeLabel srcNode
                            <> "': port is "
                            <> showCategory declared
                            <> " but edge is marked "
                            <> showCategory (edgeCategory e)
                      , veNodeId = Nothing
                      }
                  ]

    tgtCatErrors :: Edge -> [ValidationError]
    tgtCatErrors e =
      case Map.lookup (edgeTargetNode e) nodes of
        Nothing -> []
        Just tgtNode ->
          case portCategory (inputPorts (nodeType tgtNode)) (edgeTargetPort e) of
            Nothing -> []
            Just declared ->
              if declared == edgeCategory e
                then []
                else
                  [ ValidationError
                      { veMessage =
                          "Edge category mismatch on target port '"
                            <> unPortName (edgeTargetPort e)
                            <> "' of node '"
                            <> nodeLabel tgtNode
                            <> "': port is "
                            <> showCategory declared
                            <> " but edge is marked "
                            <> showCategory (edgeCategory e)
                      , veNodeId = Nothing
                      }
                  ]

-- ---------------------------------------------------------------------------
-- Check 6: Required connections wired
-- ---------------------------------------------------------------------------

checkRequiredWiring :: Graph -> [ValidationError]
checkRequiredWiring g =
  concatMap checkNode (Map.toList (graphNodes g))
  where
    -- Build set of (targetNodeId, targetPortName) pairs that have at least
    -- one incoming data edge.
    wiredDataInputs :: Set (NodeId, PortName)
    wiredDataInputs =
      Set.fromList
        [ (edgeTargetNode e, edgeTargetPort e)
        | e <- dataEdgesOf g
        ]

    checkNode :: (NodeId, Node) -> [ValidationError]
    checkNode (nid, node) =
      let required =
            [ portSpecName p
            | p <- inputPorts (nodeType node)
            , portSpecCategory p == PortData
            , not (portSpecOptional p)
            ]
      in mapMaybe (checkPort nid node) required

    checkPort :: NodeId -> Node -> PortName -> Maybe ValidationError
    checkPort nid node port =
      if Set.member (nid, port) wiredDataInputs
        then Nothing
        else
          Just $
            ValidationError
              { veMessage =
                  "Node '"
                    <> nodeLabel node
                    <> "' ("
                    <> nodeTypeName (nodeType node)
                    <> ") requires port '"
                    <> unPortName port
                    <> "' to be wired but has no incoming data edge on that port"
              , veNodeId = Just nid
              }

-- ---------------------------------------------------------------------------
-- Check 7: Config completeness
-- ---------------------------------------------------------------------------

checkConfigComplete :: Graph -> [ValidationError]
checkConfigComplete g =
  concatMap checkNode (Map.toList (graphNodes g))
  where
    checkNode :: (NodeId, Node) -> [ValidationError]
    checkNode (nid, node) = case nodeType node of
      AgentNode cfg     -> checkAgentConfig nid node cfg
      TriggerNode cfg   -> checkTriggerConfig nid node cfg
      KnowledgeNode cfg -> checkKnowledgeConfig nid node cfg
      ConnectorNode _   -> []
      ActionNode _      -> []

    mkErr :: NodeId -> Text -> ValidationError
    mkErr nid msg = ValidationError { veMessage = msg, veNodeId = Just nid }

    checkAgentConfig :: NodeId -> Node -> AgentConfig -> [ValidationError]
    checkAgentConfig nid node cfg =
      [ mkErr nid $
          "Node '"
            <> nodeLabel node
            <> "' (agent): 'model' must not be empty"
      | T.null (agentModel cfg)
      ]
        <> [ mkErr nid $
               "Node '"
                 <> nodeLabel node
                 <> "' (agent): 'systemPrompt' must not be empty"
           | T.null (agentSystemPrompt cfg)
           ]

    checkTriggerConfig :: NodeId -> Node -> TriggerConfig -> [ValidationError]
    checkTriggerConfig nid node cfg =
      case triggerType cfg of
        TriggerCron ->
          [ mkErr nid $
              "Node '"
                <> nodeLabel node
                <> "' (trigger): cron trigger requires a 'schedule' expression"
          | Nothing <- [triggerSchedule cfg]
          ]
        _ -> []

    checkKnowledgeConfig :: NodeId -> Node -> KnowledgeConfig -> [ValidationError]
    checkKnowledgeConfig nid node cfg =
      case knowledgeSource cfg of
        InlineText t ->
          [ mkErr nid $
              "Node '"
                <> nodeLabel node
                <> "' (knowledge): inline content must not be empty"
          | T.null t
          ]
        _ -> []

-- ---------------------------------------------------------------------------
-- Check 8: Reachability (Agent and Action nodes only)
-- ---------------------------------------------------------------------------

-- | Every Agent and Action node must be reachable from at least one Trigger
-- via data edges. Knowledge and Connector nodes are exempt — their outputs
-- are resource-only and they are not part of the data flow.
checkReachability :: Graph -> [ValidationError]
checkReachability g =
  let reachable = reachableViaDataEdges g
      candidates =
        [ node
        | node <- Map.elems (graphNodes g)
        , needsReachability (nodeType node)
        ]
  in mapMaybe (checkNode reachable) candidates
  where
    needsReachability :: NodeType -> Bool
    needsReachability (AgentNode _)  = True
    needsReachability (ActionNode _) = True
    needsReachability _              = False

    checkNode :: Set NodeId -> Node -> Maybe ValidationError
    checkNode reachable node =
      if Set.member (nodeId node) reachable
        then Nothing
        else
          Just $
            ValidationError
              { veMessage =
                  "Node '"
                    <> nodeLabel node
                    <> "' ("
                    <> nodeTypeName (nodeType node)
                    <> ") is not reachable from any Trigger via data edges"
              , veNodeId = Just (nodeId node)
              }

-- | BFS/DFS from all Trigger nodes following data edges only.
reachableViaDataEdges :: Graph -> Set NodeId
reachableViaDataEdges g =
  let triggerIds =
        [ nodeId n
        | n <- Map.elems (graphNodes g)
        , isTrigger (nodeType n)
        ]
      adjMap = buildDataAdjMap g
  in bfsReach adjMap (Set.fromList triggerIds) (Set.fromList triggerIds)
  where
    isTrigger (TriggerNode _) = True
    isTrigger _               = False

-- | Adjacency map: nodeId -> [downstream nodeIds] via data edges.
buildDataAdjMap :: Graph -> Map NodeId [NodeId]
buildDataAdjMap g =
  foldr
    (\e m -> Map.insertWith (++) (edgeSourceNode e) [edgeTargetNode e] m)
    (Map.fromList [(nid, []) | nid <- Map.keys (graphNodes g)])
    (dataEdgesOf g)

bfsReach :: Map NodeId [NodeId] -> Set NodeId -> Set NodeId -> Set NodeId
bfsReach adj frontier visited
  | Set.null frontier = visited
  | otherwise =
      let neighbours =
            Set.fromList $
              concatMap (\nid -> Map.findWithDefault [] nid adj) (Set.toList frontier)
          newFrontier = Set.difference neighbours visited
          newVisited  = Set.union visited newFrontier
      in bfsReach adj newFrontier newVisited

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

unNodeId :: NodeId -> Text
unNodeId (NodeId t) = t

unPortName :: PortName -> Text
unPortName (PortName t) = t

nodeTypeName :: NodeType -> Text
nodeTypeName (AgentNode _)     = "agent"
nodeTypeName (KnowledgeNode _) = "knowledge"
nodeTypeName (ConnectorNode _) = "connector"
nodeTypeName (ActionNode _)    = "action"
nodeTypeName (TriggerNode _)   = "trigger"

showCategory :: PortCategory -> Text
showCategory PortData     = "data"
showCategory PortResource = "resource"
