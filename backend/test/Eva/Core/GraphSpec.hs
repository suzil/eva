{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eva.Core.GraphSpec (spec) where

import Data.Aeson (Value (..))
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import qualified Data.Text as T

import Test.Hspec
import Test.QuickCheck

import Eva.Core.Graph
import Eva.Core.Types

-- ---------------------------------------------------------------------------
-- Minimal node/edge constructors
-- ---------------------------------------------------------------------------

mkNode :: NodeId -> NodeType -> Node
mkNode nid nt =
  Node{nodeId = nid, nodeLabel = "test", nodeType = nt, nodePosX = 0, nodePosY = 0}

mkDataEdge :: NodeId -> NodeId -> Edge
mkDataEdge src tgt =
  Edge
    { edgeId = EdgeId (unId src <> "->" <> unId tgt)
    , edgeSourceNode = src
    , edgeSourcePort = "output"
    , edgeTargetNode = tgt
    , edgeTargetPort = "input"
    , edgeCategory = PortData
    }

mkResourceEdge :: NodeId -> NodeId -> Edge
mkResourceEdge src tgt =
  Edge
    { edgeId = EdgeId (unId src <> "~>" <> unId tgt)
    , edgeSourceNode = src
    , edgeSourcePort = "content"
    , edgeTargetNode = tgt
    , edgeTargetPort = "context"
    , edgeCategory = PortResource
    }

unId :: NodeId -> T.Text
unId (NodeId t) = t

-- ---------------------------------------------------------------------------
-- Sample node types
-- ---------------------------------------------------------------------------

agentNode :: NodeType
agentNode =
  AgentNode
    AgentConfig
      { agentModel = "gpt-4o"
      , agentSystemPrompt = "test"
      , agentResponseFormat = ResponseText
      , agentTemperature = 0.5
      , agentMaxTokens = Nothing
      , agentMaxIterations = 3
      , agentCostBudgetUsd = Nothing
      }

actionNode :: NodeType
actionNode =
  ActionNode
    ActionConfig
      { actionOperation = OpTemplate
      , actionParameters = Null
      , actionErrorHandling = ErrFail
      }

triggerNode :: NodeType
triggerNode =
  TriggerNode
    TriggerConfig
      { triggerType = TriggerManual
      , triggerSchedule = Nothing
      , triggerEventFilter = Nothing
      , triggerPayloadTemplate = Nothing
      }

knowledgeNode :: NodeType
knowledgeNode =
  KnowledgeNode
    KnowledgeConfig
      { knowledgeSource = InlineText "context"
      , knowledgeFormat = FormatText
      , knowledgeRefreshPolicy = RefreshStatic
      }

connectorNode :: NodeType
connectorNode =
  ConnectorNode
    ConnectorConfig
      { connectorSystem = SystemLinear
      , connectorCredentialId = Nothing
      , connectorEndpoint = Nothing
      , connectorScope = Nothing
      , connectorActionFilter = []
      }

-- ---------------------------------------------------------------------------
-- Hand-built fixture graphs
-- ---------------------------------------------------------------------------

-- | A --data--> B --data--> C
linearChain :: Graph
linearChain =
  Graph
    { graphNodes =
        Map.fromList
          [ ("A", mkNode "A" triggerNode)
          , ("B", mkNode "B" actionNode)
          , ("C", mkNode "C" actionNode)
          ]
    , graphEdges = [mkDataEdge "A" "B", mkDataEdge "B" "C"]
    }

-- | A -> B, A -> C, B -> D, C -> D  (diamond)
diamond :: Graph
diamond =
  Graph
    { graphNodes =
        Map.fromList
          [ ("A", mkNode "A" triggerNode)
          , ("B", mkNode "B" actionNode)
          , ("C", mkNode "C" actionNode)
          , ("D", mkNode "D" agentNode)
          ]
    , graphEdges =
        [ mkDataEdge "A" "B"
        , mkDataEdge "A" "C"
        , mkDataEdge "B" "D"
        , mkDataEdge "C" "D"
        ]
    }

-- | A --data--> B --data--> A  (2-node cycle)
withCycle :: Graph
withCycle =
  Graph
    { graphNodes =
        Map.fromList
          [ ("A", mkNode "A" actionNode)
          , ("B", mkNode "B" actionNode)
          ]
    , graphEdges = [mkDataEdge "A" "B", mkDataEdge "B" "A"]
    }

-- | A -> B -> C -> A  (3-node cycle)
longerCycle :: Graph
longerCycle =
  Graph
    { graphNodes =
        Map.fromList
          [ ("A", mkNode "A" actionNode)
          , ("B", mkNode "B" actionNode)
          , ("C", mkNode "C" actionNode)
          ]
    , graphEdges =
        [ mkDataEdge "A" "B"
        , mkDataEdge "B" "C"
        , mkDataEdge "C" "A"
        ]
    }

-- | Knowledge --resource--> Agent only.  No data edges at all.
resourceOnly :: Graph
resourceOnly =
  Graph
    { graphNodes =
        Map.fromList
          [ ("K", mkNode "K" knowledgeNode)
          , ("Ag", mkNode "Ag" agentNode)
          ]
    , graphEdges = [mkResourceEdge "K" "Ag"]
    }

-- | Trigger --data--> Agent,  Knowledge --resource--> Agent.
mixed :: Graph
mixed =
  Graph
    { graphNodes =
        Map.fromList
          [ ("T", mkNode "T" triggerNode)
          , ("K", mkNode "K" knowledgeNode)
          , ("Ag", mkNode "Ag" agentNode)
          ]
    , graphEdges =
        [ mkDataEdge "T" "Ag"
        , mkResourceEdge "K" "Ag"
        ]
    }

-- | A single isolated node (TriggerNode — no required data inputs).
singleNode :: Graph
singleNode =
  Graph
    { graphNodes = Map.singleton "X" (mkNode "X" triggerNode)
    , graphEdges = []
    }

-- ---------------------------------------------------------------------------
-- DAG generator for property tests
-- ---------------------------------------------------------------------------

-- | A random DAG guaranteed to have no cycles.
-- Constructed by assigning each node an integer rank and only creating data
-- edges from lower-ranked to higher-ranked nodes.
newtype DagGraph = DagGraph Graph

instance Show DagGraph where
  show (DagGraph g) =
    "DagGraph {"
      <> show (length (Map.keys (graphNodes g)))
      <> " nodes, "
      <> show (length (graphEdges g))
      <> " edges}"

instance Arbitrary DagGraph where
  arbitrary = do
    n <- choose (1, 8 :: Int)
    let nodeIds = map (\i -> NodeId ("n" <> T.pack (show i))) [1 .. n]
    let nodes = Map.fromList [(nid, mkNode nid actionNode) | nid <- nodeIds]
    edges <-
      fmap concat $
        sequence
          [ do
              addEdge <- arbitrary
              if addEdge
                then pure [mkDataEdge (nodeIds !! i) (nodeIds !! j)]
                else pure []
          | i <- [0 .. n - 2]
          , j <- [i + 1 .. n - 1]
          ]
    pure $ DagGraph $ Graph nodes edges

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | True iff for every data edge (u -> v), u appears before v in the list.
respectsEdgeOrder :: [NodeId] -> [Edge] -> Bool
respectsEdgeOrder order edges =
  all check (filter (\e -> edgeCategory e == PortData) edges)
  where
    posMap = Map.fromList (zip order [0 :: Int ..])
    check e =
      case (Map.lookup (edgeSourceNode e) posMap, Map.lookup (edgeTargetNode e) posMap) of
        (Just i, Just j) -> i < j
        _ -> True

pos :: [NodeId] -> NodeId -> Int
pos order nid = Map.fromList (zip order [0 :: Int ..]) Map.! nid

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Eva.Core.Graph" $ do
  -- -------------------------------------------------------------------------
  describe "dataEdgesOf" $ do
    it "returns only data edges from mixed graph" $ do
      let edges = dataEdgesOf mixed
      all (\e -> edgeCategory e == PortData) edges `shouldBe` True
      length edges `shouldBe` 1

    it "returns empty list for resource-only graph" $
      dataEdgesOf resourceOnly `shouldBe` []

    it "returns all data edges when there are no resource edges" $
      length (dataEdgesOf linearChain) `shouldBe` 2

  -- -------------------------------------------------------------------------
  describe "topoSort" $ do
    it "linear chain A→B→C returns [A,B,C]" $
      topoSort linearChain `shouldBe` Right ["A", "B", "C"]

    it "diamond: A before B, C; B and C before D" $
      case topoSort diamond of
        Left _ -> expectationFailure "unexpected cycle"
        Right order -> do
          pos order "A" `shouldSatisfy` (< pos order "B")
          pos order "A" `shouldSatisfy` (< pos order "C")
          pos order "B" `shouldSatisfy` (< pos order "D")
          pos order "C" `shouldSatisfy` (< pos order "D")

    it "single isolated node returns that node" $
      topoSort singleNode `shouldBe` Right ["X"]

    it "2-node cycle returns Left with both nodes" $
      case topoSort withCycle of
        Left cyclePath -> sort cyclePath `shouldBe` ["A", "B"]
        Right _ -> expectationFailure "expected Left (cycle)"

    it "3-node cycle returns Left" $
      topoSort longerCycle `shouldSatisfy` \case Left _ -> True; Right _ -> False

    it "resource-only graph: both nodes in result, no cycle" $
      case topoSort resourceOnly of
        Left _ -> expectationFailure "unexpected cycle"
        Right order -> sort order `shouldBe` sort ["K", "Ag"]

    it "mixed graph: Trigger before Agent in data order" $
      case topoSort mixed of
        Left _ -> expectationFailure "unexpected cycle"
        Right order -> pos order "T" `shouldSatisfy` (< pos order "Ag")

  -- -------------------------------------------------------------------------
  describe "detectCycle" $ do
    it "returns Nothing for a linear chain" $
      detectCycle linearChain `shouldBe` Nothing

    it "returns Nothing for a diamond" $
      detectCycle diamond `shouldBe` Nothing

    it "returns Nothing for resource-only graph" $
      detectCycle resourceOnly `shouldBe` Nothing

    it "returns Nothing for a single node" $
      detectCycle singleNode `shouldBe` Nothing

    it "returns Just for a 2-node cycle" $
      detectCycle withCycle `shouldSatisfy` isJust

    it "cycle path contains both cycling nodes" $
      sort (fromJust (detectCycle withCycle)) `shouldBe` ["A", "B"]

    it "returns Just for a 3-node cycle" $
      detectCycle longerCycle `shouldSatisfy` isJust

    it "resource edges do not create false cycles" $
      detectCycle resourceOnly `shouldBe` Nothing

  -- -------------------------------------------------------------------------
  describe "rootNodes" $ do
    it "linear chain: only A is a root" $
      rootNodes linearChain `shouldBe` ["A"]

    it "diamond: only A is a root" $
      rootNodes diamond `shouldBe` ["A"]

    it "single node: that node is a root" $
      rootNodes singleNode `shouldBe` ["X"]

    it "resource-only graph: both nodes are data roots (no data edges)" $
      sort (rootNodes resourceOnly) `shouldBe` sort ["K", "Ag"]

    it "mixed graph: Trigger and Knowledge are data roots" $
      sort (rootNodes mixed) `shouldBe` sort ["T", "K"]

  -- -------------------------------------------------------------------------
  describe "terminalNodes" $ do
    it "linear chain: only C is a terminal" $
      terminalNodes linearChain `shouldBe` ["C"]

    it "diamond: only D is a terminal" $
      terminalNodes diamond `shouldBe` ["D"]

    it "single node: that node is a terminal" $
      terminalNodes singleNode `shouldBe` ["X"]

    it "resource-only graph: both nodes are data terminals (no data edges)" $
      sort (terminalNodes resourceOnly) `shouldBe` sort ["K", "Ag"]

    it "mixed graph: Knowledge and Agent are terminals (no outgoing data edges)" $
      sort (terminalNodes mixed) `shouldBe` sort ["K", "Ag"]

  -- -------------------------------------------------------------------------
  describe "requiredDataInputs" $ do
    it "AgentNode requires 'instruction'" $
      requiredDataInputs agentNode `shouldBe` ["instruction"]

    it "ActionNode requires 'input'" $
      requiredDataInputs actionNode `shouldBe` ["input"]

    it "TriggerNode has no required data inputs" $
      requiredDataInputs triggerNode `shouldBe` []

    it "KnowledgeNode has no required data inputs" $
      requiredDataInputs knowledgeNode `shouldBe` []

    it "ConnectorNode has no required data inputs" $
      requiredDataInputs connectorNode `shouldBe` []

  -- -------------------------------------------------------------------------
  describe "readyNodes" $ do
    it "empty mailbox: nodes with no required inputs are ready" $
      -- T (Trigger), K (Knowledge) have no required inputs; Ag (Agent) needs 'instruction'
      sort (readyNodes mixed Set.empty) `shouldBe` sort ["T", "K"]

    it "Agent becomes ready when 'instruction' port is filled" $
      readyNodes mixed (Set.singleton ("Ag", "instruction")) `shouldSatisfy` elem "Ag"

    it "Agent not ready when 'instruction' port is absent" $
      readyNodes mixed Set.empty `shouldSatisfy` notElem "Ag"

    it "all nodes ready when all required ports are filled" $
      sort (readyNodes mixed (Set.singleton ("Ag", "instruction")))
        `shouldBe` sort ["T", "K", "Ag"]

    it "single node with no required inputs is always ready" $
      readyNodes singleNode Set.empty `shouldBe` ["X"]

    it "ActionNode ready only when 'input' port is filled" $ do
      -- linearChain: A=Trigger (always ready), B=Action, C=Action
      readyNodes linearChain Set.empty `shouldSatisfy` notElem ("B" :: NodeId)
      readyNodes linearChain (Set.singleton ("B", "input")) `shouldSatisfy` elem "B"

    it "multiple action nodes: each independently requires its own 'input'" $ do
      let filled = Set.fromList [("B", "input"), ("C", "input")]
      readyNodes linearChain filled `shouldSatisfy` \r -> elem "B" r && elem "C" r

  -- -------------------------------------------------------------------------
  describe "property tests" $ do
    it "topoSort output respects all data edge ordering constraints" $
      property $ \(DagGraph g) ->
        case topoSort g of
          Left _ -> property False
          Right order -> property (respectsEdgeOrder order (graphEdges g))

    it "topoSort output contains every node exactly once" $
      property $ \(DagGraph g) ->
        case topoSort g of
          Left _ -> property False
          Right order ->
            sort order === sort (Map.keys (graphNodes g))
