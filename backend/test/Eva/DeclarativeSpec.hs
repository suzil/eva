{-# LANGUAGE OverloadedStrings #-}

module Eva.DeclarativeSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eva.Core.Types
import Eva.Declarative

-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

agentCfg :: AgentConfig
agentCfg =
  AgentConfig
    { agentProvider       = Just ProviderOpenAI
    , agentModel          = "gpt-4o"
    , agentSystemPrompt   = "Summarize the input.\nBe concise."
    , agentResponseFormat = ResponseText
    , agentTemperature    = 0.7
    , agentMaxTokens      = Just 1024
    , agentMaxIterations  = 3
    , agentCostBudgetUsd  = Nothing
    , agentRetryPolicy    = Nothing
    }

knowledgeCfg :: KnowledgeConfig
knowledgeCfg =
  KnowledgeConfig
    { knowledgeSource        = InlineText "Team goals: ship EVA phase 2 by May."
    , knowledgeFormat        = FormatText
    , knowledgeRefreshPolicy = RefreshStatic
    }

connectorCfg :: ConnectorConfig
connectorCfg =
  ConnectorConfig
    { connectorSystem       = SystemLinear
    , connectorCredentialId = Just "cred-linear-1"
    , connectorEndpoint     = Nothing
    , connectorScope        = Nothing
    , connectorActionFilter = ["list_issues", "create_issue"]
    }

actionCfg :: ActionConfig
actionCfg =
  ActionConfig
    { actionOperation     = OpTemplate
    , actionParameters    = Object mempty
    , actionErrorHandling = ErrContinue
    , actionRetryPolicy   = Nothing
    }

triggerCfg :: TriggerConfig
triggerCfg =
  TriggerConfig
    { triggerType            = TriggerCron
    , triggerSchedule        = Just "0 9 * * 1"
    , triggerEventFilter     = Nothing
    , triggerPayloadTemplate = Nothing
    }

mkNode :: NodeId -> Text -> NodeType -> Node
mkNode nid lbl nt =
  Node { nodeId = nid, nodeLabel = lbl, nodeType = nt, nodePosX = 100.0, nodePosY = 200.0 }

-- | Single-node graph (no edges).
singleNodeGraph :: NodeType -> Graph
singleNodeGraph nt =
  Graph
    { graphNodes = Map.singleton "n1" (mkNode "n1" "Test Node" nt)
    , graphEdges = []
    }

-- | Two-node graph with one data edge.
twoNodeGraph :: Graph
twoNodeGraph =
  Graph
    { graphNodes = Map.fromList
        [ ("t1", mkNode "t1" "Trigger" (TriggerNode triggerCfg))
        , ("a1", mkNode "a1" "Agent"   (AgentNode agentCfg))
        ]
    , graphEdges =
        [ Edge
            { edgeId         = "e-t1-a1"
            , edgeSourceNode = "t1"
            , edgeSourcePort = "output"
            , edgeTargetNode = "a1"
            , edgeTargetPort = "input"
            , edgeCategory   = PortData
            }
        ]
    }

-- | Weekly Summarizer demo: Trigger + Knowledge + Agent (3 nodes, 2 edges).
weeklySummarizerGraph :: Graph
weeklySummarizerGraph =
  Graph
    { graphNodes = Map.fromList
        [ ("trigger-1", mkNode "trigger-1" "Weekly Trigger" (TriggerNode triggerCfg))
        , ("know-1",    mkNode "know-1"    "Team Context"   (KnowledgeNode knowledgeCfg))
        , ("agent-1",   mkNode "agent-1"   "Summarizer"     (AgentNode agentCfg))
        ]
    , graphEdges =
        [ Edge "e-t-a" "trigger-1" "output"  "agent-1" "input"   PortData
        , Edge "e-k-a" "know-1"    "content" "agent-1" "context" PortResource
        ]
    }

-- ---------------------------------------------------------------------------
-- Round-trip helper
-- ---------------------------------------------------------------------------

roundTrip :: Graph -> IO ()
roundTrip g = do
  let yaml   = graphToYaml g
      result = yamlToGraph yaml
  result `shouldBe` Right g

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Eva.Declarative" $ do

    describe "graphToYaml" $ do
      it "produces non-empty YAML text" $ do
        let yaml = graphToYaml (singleNodeGraph (AgentNode agentCfg))
        T.null yaml `shouldBe` False

      it "includes eva.version header" $ do
        let yaml = graphToYaml (singleNodeGraph (AgentNode agentCfg))
        yaml `shouldSatisfy` ("eva.version" `T.isInfixOf`)

      it "uses 'triggerType' key (not 'type') for trigger config" $ do
        let yaml = graphToYaml (singleNodeGraph (TriggerNode triggerCfg))
        yaml `shouldSatisfy` ("triggerType" `T.isInfixOf`)

      it "does not duplicate 'type: trigger' in trigger node config" $ do
        let yaml = graphToYaml (singleNodeGraph (TriggerNode triggerCfg))
        -- 'type: trigger' appears once (as the node type) — triggerType key is separate
        let occurrences needle hay = length (T.breakOnAll needle hay)
        occurrences "triggerType" yaml `shouldBe` 1

    describe "round-trip fidelity" $ do
      it "agent node (provider, maxTokens set)" $
        roundTrip (singleNodeGraph (AgentNode agentCfg))

      it "agent node (minimal, no optional fields)" $
        roundTrip
          (singleNodeGraph
            (AgentNode agentCfg
              { agentProvider      = Nothing
              , agentMaxTokens     = Nothing
              , agentCostBudgetUsd = Nothing
              , agentRetryPolicy   = Nothing
              }))

      it "knowledge node (inline text, static refresh)" $
        roundTrip (singleNodeGraph (KnowledgeNode knowledgeCfg))

      it "knowledge node (upstream port source)" $
        roundTrip
          (singleNodeGraph
            (KnowledgeNode knowledgeCfg{knowledgeSource = UpstreamPort}))

      it "knowledge node (periodic refresh)" $
        roundTrip
          (singleNodeGraph
            (KnowledgeNode knowledgeCfg{knowledgeRefreshPolicy = RefreshPeriodic 3600}))

      it "connector node" $
        roundTrip (singleNodeGraph (ConnectorNode connectorCfg))

      it "connector node (all optional fields)" $
        roundTrip
          (singleNodeGraph
            (ConnectorNode connectorCfg
              { connectorEndpoint = Just "https://api.linear.app/graphql"
              , connectorScope    = Just "project:EVA"
              }))

      it "action node" $
        roundTrip (singleNodeGraph (ActionNode actionCfg))

      it "action node (errorHandling use_default)" $
        roundTrip
          (singleNodeGraph
            (ActionNode actionCfg{actionErrorHandling = ErrUseDefault "fallback"}))

      it "trigger node (cron)" $
        roundTrip (singleNodeGraph (TriggerNode triggerCfg))

      it "trigger node (manual, no schedule)" $
        roundTrip
          (singleNodeGraph
            (TriggerNode triggerCfg
              { triggerType     = TriggerManual
              , triggerSchedule = Nothing
              }))

      it "agent with exponential backoff retry policy" $
        roundTrip
          (singleNodeGraph
            (AgentNode agentCfg
              { agentRetryPolicy = Just
                  RetryPolicy
                    { retryMaxAttempts = 3
                    , retryBackoff     = BackoffExponential 1000 30000
                    , retryTimeoutMs   = Just 60000
                    }
              }))

      it "two-node graph with one data edge" $
        roundTrip twoNodeGraph

      it "Weekly Summarizer demo graph (trigger + knowledge + agent)" $
        roundTrip weeklySummarizerGraph

      it "empty graph (no nodes, no edges)" $
        roundTrip (Graph Map.empty [])

    describe "error handling" $ do
      it "returns Left for syntactically invalid YAML" $ do
        let result = yamlToGraph "{ unclosed: ["
        result `shouldSatisfy` isLeft

      it "returns Left for missing required 'model' field on agent node" $ do
        let badYaml = T.unlines
              [ "eva.version: \"1\""
              , "nodes:"
              , "  n1:"
              , "    type: agent"
              , "    label: Bad Agent"
              , "    posX: 0.0"
              , "    posY: 0.0"
              , "    systemPrompt: hello"
              , "    responseFormat: text"
              , "    temperature: 0.7"
              , "    maxIterations: 3"
              , "    # 'model' field intentionally missing"
              , "edges: []"
              ]
        yamlToGraph badYaml `shouldSatisfy` isLeft

      it "returns Left for unknown node type" $ do
        let badYaml = T.unlines
              [ "eva.version: \"1\""
              , "nodes:"
              , "  n1:"
              , "    type: unknown_type"
              , "    label: Bad"
              , "    posX: 0.0"
              , "    posY: 0.0"
              , "edges: []"
              ]
        yamlToGraph badYaml `shouldSatisfy` isLeft

      it "returns Left for unknown edge category" $ do
        let badYaml = T.unlines
              [ "eva.version: \"1\""
              , "nodes: {}"
              , "edges:"
              , "  - from: n1"
              , "    fromPort: out"
              , "    to: n2"
              , "    toPort: in"
              , "    category: invalid_cat"
              ]
        yamlToGraph badYaml `shouldSatisfy` isLeft

      it "error messages are non-empty strings" $ do
        let result = yamlToGraph "{ not valid yaml: ["
        case result of
          Right _   -> expectationFailure "expected Left"
          Left errs -> do
            errs `shouldSatisfy` (not . null)
            all (\(ParseError msg) -> not (T.null msg)) errs `shouldBe` True

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False
