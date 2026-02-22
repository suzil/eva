{-# LANGUAGE OverloadedStrings #-}

module Eva.Core.ValidationSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Test.Hspec

import Eva.Core.Types
import Eva.Core.Validation (validateGraph)

-- ---------------------------------------------------------------------------
-- Node / edge constructors
-- ---------------------------------------------------------------------------

mkNode :: NodeId -> Text -> NodeType -> Node
mkNode nid lbl nt =
  Node{nodeId = nid, nodeLabel = lbl, nodeType = nt, nodePosX = 0, nodePosY = 0}

mkEdge :: EdgeId -> NodeId -> PortName -> NodeId -> PortName -> PortCategory -> Edge
mkEdge eid src sp tgt tp cat =
  Edge
    { edgeId = eid
    , edgeSourceNode = src
    , edgeSourcePort = sp
    , edgeTargetNode = tgt
    , edgeTargetPort = tp
    , edgeCategory = cat
    }

mkGraph :: [Node] -> [Edge] -> Graph
mkGraph ns es =
  Graph
    { graphNodes = Map.fromList [(nodeId n, n) | n <- ns]
    , graphEdges = es
    }

-- ---------------------------------------------------------------------------
-- Sample node types
-- ---------------------------------------------------------------------------

mkTrigger :: NodeType
mkTrigger =
  TriggerNode
    TriggerConfig
      { triggerType = TriggerManual
      , triggerSchedule = Nothing
      , triggerEventFilter = Nothing
      , triggerPayloadTemplate = Nothing
      }

mkCronTrigger :: Maybe Text -> NodeType
mkCronTrigger sched =
  TriggerNode
    TriggerConfig
      { triggerType = TriggerCron
      , triggerSchedule = sched
      , triggerEventFilter = Nothing
      , triggerPayloadTemplate = Nothing
      }

mkAgent :: Text -> Text -> NodeType
mkAgent model prompt =
  AgentNode
    AgentConfig
      { agentModel = model
      , agentSystemPrompt = prompt
      , agentResponseFormat = ResponseText
      , agentTemperature = 0.7
      , agentMaxTokens = Nothing
      , agentMaxIterations = 3
      , agentCostBudgetUsd = Nothing
      }

mkAction :: NodeType
mkAction =
  ActionNode
    ActionConfig
      { actionOperation = OpTemplate
      , actionParameters = Null
      , actionErrorHandling = ErrFail
      }

mkKnowledge :: Text -> NodeType
mkKnowledge content =
  KnowledgeNode
    KnowledgeConfig
      { knowledgeSource = InlineText content
      , knowledgeFormat = FormatText
      , knowledgeRefreshPolicy = RefreshStatic
      }

mkConnector :: NodeType
mkConnector =
  ConnectorNode
    ConnectorConfig
      { connectorSystem = SystemLinear
      , connectorCredentialId = Nothing
      , connectorEndpoint = Nothing
      , connectorScope = Nothing
      , connectorActionFilter = []
      }

-- ---------------------------------------------------------------------------
-- Shared minimal valid graph: Trigger -> Agent
-- ---------------------------------------------------------------------------

validGraph :: Graph
validGraph =
  mkGraph
    [ mkNode "t1" "Trigger"    mkTrigger
    , mkNode "a1" "Summariser" (mkAgent "gpt-4o" "Summarise.")
    ]
    [ mkEdge "e1" "t1" "event" "a1" "instruction" PortData
    ]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "validateGraph" $ do
  -- ------------------------------------------------------------------
  describe "valid graphs" $ do
    it "passes a minimal Trigger -> Agent graph" $
      validateGraph validGraph `shouldBe` []

    it "passes a graph with Knowledge connected only via resource edge" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger" mkTrigger
              , mkNode "k1" "TeamCtx" (mkKnowledge "some context")
              , mkNode "a1" "Agent"   (mkAgent "gpt-4o" "Summarise.")
              ]
              [ mkEdge "e1" "t1" "event"   "a1" "instruction" PortData
              , mkEdge "e2" "k1" "content" "a1" "context"     PortResource
              ]
      validateGraph g `shouldBe` []

    it "passes a graph with a Connector wired to Agent tools" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger"   mkTrigger
              , mkNode "c1" "Linear"    mkConnector
              , mkNode "a1" "Agent"     (mkAgent "gpt-4o" "Do things.")
              ]
              [ mkEdge "e1" "t1" "event" "a1" "instruction" PortData
              , mkEdge "e2" "c1" "tools" "a1" "tools"       PortResource
              ]
      validateGraph g `shouldBe` []

    it "passes a Trigger -> Agent -> Action chain" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger" mkTrigger
              , mkNode "a1" "Agent"   (mkAgent "gpt-4o" "Write.")
              , mkNode "ac" "Format"  mkAction
              ]
              [ mkEdge "e1" "t1" "event"  "a1" "instruction" PortData
              , mkEdge "e2" "a1" "output" "ac" "input"       PortData
              ]
      validateGraph g `shouldBe` []

  -- ------------------------------------------------------------------
  describe "trigger presence" $ do
    it "rejects a graph with no Trigger node" $ do
      let g = mkGraph [mkNode "a1" "Agent" (mkAgent "gpt-4o" "x")] []
      veMessage <$> validateGraph g
        `shouldContain` ["Graph must contain at least one Trigger node"]

  -- ------------------------------------------------------------------
  describe "DAG check" $ do
    it "rejects a graph with a cycle in data edges" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger" mkTrigger
              , mkNode "a1" "Agent"   (mkAgent "gpt-4o" "x")
              , mkNode "a2" "Agent2"  (mkAgent "gpt-4o" "y")
              ]
              [ mkEdge "e1" "t1" "event"  "a1" "instruction" PortData
              , mkEdge "e2" "a1" "output" "a2" "instruction" PortData
              , mkEdge "e3" "a2" "output" "a1" "instruction" PortData  -- cycle
              ]
      let errs = veMessage <$> validateGraph g
      errs `shouldSatisfy` any (T.isPrefixOf "Cycle detected")

  -- ------------------------------------------------------------------
  describe "edge port name validation" $ do
    it "rejects an edge referencing a non-existent source port" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger" mkTrigger
              , mkNode "a1" "Agent"   (mkAgent "gpt-4o" "x")
              ]
              [ mkEdge "e1" "t1" "bogus_port" "a1" "instruction" PortData
              ]
      let errs = veMessage <$> validateGraph g
      errs `shouldSatisfy` any (T.isInfixOf "Edge source port 'bogus_port'")

    it "rejects an edge referencing a non-existent target port" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger" mkTrigger
              , mkNode "a1" "Agent"   (mkAgent "gpt-4o" "x")
              ]
              [ mkEdge "e1" "t1" "event" "a1" "bogus_port" PortData
              ]
      let errs = veMessage <$> validateGraph g
      errs `shouldSatisfy` any (T.isInfixOf "Edge target port 'bogus_port'")

  -- ------------------------------------------------------------------
  describe "port category compatibility" $ do
    it "rejects a data edge on a resource-category port" $ do
      -- Connector's 'tools' output is resource; marking the edge as PortData is wrong
      let g =
            mkGraph
              [ mkNode "t1" "Trigger"   mkTrigger
              , mkNode "c1" "Connector" mkConnector
              , mkNode "a1" "Agent"     (mkAgent "gpt-4o" "x")
              ]
              [ mkEdge "e1" "t1" "event" "a1" "instruction" PortData
              , mkEdge "e2" "c1" "tools" "a1" "tools"       PortData  -- should be PortResource
              ]
      let errs = veMessage <$> validateGraph g
      errs `shouldSatisfy` any (T.isInfixOf "Edge category mismatch")

  -- ------------------------------------------------------------------
  describe "required connections" $ do
    it "rejects an Agent node with no incoming instruction edge" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger" mkTrigger
              , mkNode "a1" "Agent"   (mkAgent "gpt-4o" "x")
              ]
              []
      let errs = veMessage <$> validateGraph g
      errs `shouldSatisfy` any (T.isInfixOf "requires port 'instruction'")

    it "rejects an Action node with no incoming input edge" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger" mkTrigger
              , mkNode "ac" "Format"  mkAction
              ]
              []
      let errs = veMessage <$> validateGraph g
      errs `shouldSatisfy` any (T.isInfixOf "requires port 'input'")

  -- ------------------------------------------------------------------
  describe "config completeness" $ do
    it "rejects an Agent with an empty model field" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger" mkTrigger
              , mkNode "a1" "Agent"   (mkAgent "" "Some prompt")
              ]
              [ mkEdge "e1" "t1" "event" "a1" "instruction" PortData
              ]
      let errs = veMessage <$> validateGraph g
      errs `shouldSatisfy` any (T.isInfixOf "'model' must not be empty")

    it "rejects an Agent with an empty systemPrompt field" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger" mkTrigger
              , mkNode "a1" "Agent"   (mkAgent "gpt-4o" "")
              ]
              [ mkEdge "e1" "t1" "event" "a1" "instruction" PortData
              ]
      let errs = veMessage <$> validateGraph g
      errs `shouldSatisfy` any (T.isInfixOf "'systemPrompt' must not be empty")

    it "rejects a Cron trigger with no schedule expression" $ do
      let g =
            mkGraph
              [ mkNode "t1" "CronTrigger" (mkCronTrigger Nothing)
              , mkNode "a1" "Agent"       (mkAgent "gpt-4o" "x")
              ]
              [ mkEdge "e1" "t1" "event" "a1" "instruction" PortData
              ]
      let errs = veMessage <$> validateGraph g
      errs `shouldSatisfy` any (T.isInfixOf "cron trigger requires a 'schedule'")

    it "passes a Cron trigger that has a schedule expression" $ do
      let g =
            mkGraph
              [ mkNode "t1" "CronTrigger" (mkCronTrigger (Just "0 9 * * 1"))
              , mkNode "a1" "Agent"       (mkAgent "gpt-4o" "x")
              ]
              [ mkEdge "e1" "t1" "event" "a1" "instruction" PortData
              ]
      validateGraph g `shouldBe` []

  -- ------------------------------------------------------------------
  describe "reachability" $ do
    it "passes when all Agent nodes are reachable via data edges" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger" mkTrigger
              , mkNode "a1" "Agent"   (mkAgent "gpt-4o" "x")
              , mkNode "a2" "Agent2"  (mkAgent "gpt-4o" "y")
              ]
              [ mkEdge "e1" "t1" "event"  "a1" "instruction" PortData
              , mkEdge "e2" "a1" "output" "a2" "instruction" PortData
              ]
      validateGraph g `shouldBe` []

    it "rejects an Agent with no data-edge path from any Trigger" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger"  mkTrigger
              , mkNode "a1" "Agent"    (mkAgent "gpt-4o" "x")
              , mkNode "a2" "Isolated" (mkAgent "gpt-4o" "y")
              ]
              [ mkEdge "e1" "t1" "event" "a1" "instruction" PortData
              -- a2 has no incoming data edges; instruction also unwired
              ]
      let errs = veMessage <$> validateGraph g
      errs `shouldSatisfy` any (T.isInfixOf "not reachable from any Trigger")

    it "does not flag a Knowledge node connected only via resource edge" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger" mkTrigger
              , mkNode "k1" "Context" (mkKnowledge "some text")
              , mkNode "a1" "Agent"   (mkAgent "gpt-4o" "x")
              ]
              [ mkEdge "e1" "t1" "event"   "a1" "instruction" PortData
              , mkEdge "e2" "k1" "content" "a1" "context"     PortResource
              ]
      validateGraph g `shouldBe` []

    it "does not flag a Connector node as unreachable" $ do
      let g =
            mkGraph
              [ mkNode "t1" "Trigger"   mkTrigger
              , mkNode "c1" "Connector" mkConnector
              , mkNode "a1" "Agent"     (mkAgent "gpt-4o" "x")
              ]
              [ mkEdge "e1" "t1" "event" "a1" "instruction" PortData
              , mkEdge "e2" "c1" "tools" "a1" "tools"       PortResource
              ]
      validateGraph g `shouldBe` []
