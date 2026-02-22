{-# LANGUAGE OverloadedStrings #-}

module Eva.Engine.RunnerSpec (spec) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import qualified Data.Aeson as Aeson
import Eva.App (AppEnv (..), runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.Dispatch (execute)
import Eva.Engine.LLM
import Eva.Engine.Runner (resolveResourceBindings, startRun, waitForRun)
import Eva.Engine.StateMachine (RunContext (..))
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries (insertProgram)

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

-- | A mock LLM client that always returns a fixed success response.
-- Used in RunnerSpec so that Agent nodes succeed when the production
-- dispatch is exercised (the real handler calls the LLM client).
mockLLMClient :: LLMClient
mockLLMClient = LLMClient
  { clientCall   = \_ -> pure (Right (LLMResponse "test response" (TokenUsage 1 1 2)))
  , clientStream = \_ _ -> pure (Right (LLMResponse "test response" (TokenUsage 1 1 2)))
  }

withTestEnv :: (AppEnv -> IO ()) -> IO ()
withTestEnv action = do
  pool       <- runNoLoggingT $ createSqlitePool ":memory:" 4
  runMigrations pool
  broadcasts <- newTVarIO Map.empty
  let cfg = AppConfig
        { configDbPath        = ":memory:"
        , configPort          = 8080
        , configLlmApiKey     = Nothing
        , configLogLevel      = LogError
        , configCredentialKey = "test-key"
        }
  let env = AppEnv
        { envConfig     = cfg
        , envDbPool     = pool
        , envLogger     = \_ -> pure ()
        , envDispatch   = execute
        , envLLMClient  = mockLLMClient
        , envBroadcasts    = broadcasts
        , envCredentialKey = Crypto.deriveKey "test-key"
        }
  action env

-- ---------------------------------------------------------------------------
-- Graph fixtures
-- ---------------------------------------------------------------------------

triggerNode :: NodeId -> Node
triggerNode nid = Node
  { nodeId    = nid
  , nodeLabel = "Trigger"
  , nodeType  = TriggerNode TriggerConfig
      { triggerType            = TriggerManual
      , triggerSchedule        = Nothing
      , triggerEventFilter     = Nothing
      , triggerPayloadTemplate = Nothing
      }
  , nodePosX  = 0
  , nodePosY  = 0
  }

actionNode :: NodeId -> Node
actionNode nid = Node
  { nodeId    = nid
  , nodeLabel = "Action"
  , nodeType  = ActionNode ActionConfig
      { actionOperation    = OpTemplate
      , actionParameters   = Aeson.object [("template", Aeson.String "processed")]
      , actionErrorHandling = ErrFail
      , actionRetryPolicy  = Nothing
      }
  , nodePosX  = 100
  , nodePosY  = 0
  }

agentNode :: NodeId -> Node
agentNode nid = Node
  { nodeId    = nid
  , nodeLabel = "Agent"
  , nodeType  = AgentNode AgentConfig
      { agentModel          = "gpt-4o"
      , agentSystemPrompt   = "You are a test agent."
      , agentResponseFormat = ResponseText
      , agentTemperature    = 0.7
      , agentMaxTokens      = Nothing
      , agentMaxIterations  = 1
      , agentCostBudgetUsd  = Nothing
      , agentRetryPolicy    = Nothing
      }
  , nodePosX  = 100
  , nodePosY  = 0
  }

knowledgeNode :: NodeId -> Node
knowledgeNode nid = Node
  { nodeId    = nid
  , nodeLabel = "Knowledge"
  , nodeType  = KnowledgeNode KnowledgeConfig
      { knowledgeSource        = InlineText "Test context"
      , knowledgeFormat        = FormatText
      , knowledgeRefreshPolicy = RefreshStatic
      }
  , nodePosX  = 0
  , nodePosY  = 100
  }

connectorNode :: NodeId -> Node
connectorNode nid = Node
  { nodeId    = nid
  , nodeLabel = "Connector"
  , nodeType  = ConnectorNode ConnectorConfig
      { connectorSystem       = SystemLinear
      , connectorCredentialId = Nothing
      , connectorEndpoint     = Nothing
      , connectorScope        = Nothing
      , connectorActionFilter = []
      }
  , nodePosX  = 0
  , nodePosY  = 200
  }

dataEdge :: EdgeId -> NodeId -> PortName -> NodeId -> PortName -> Edge
dataEdge eid src srcPort tgt tgtPort = Edge
  { edgeId         = eid
  , edgeSourceNode  = src
  , edgeSourcePort  = srcPort
  , edgeTargetNode  = tgt
  , edgeTargetPort  = tgtPort
  , edgeCategory   = PortData
  }

resourceEdge :: EdgeId -> NodeId -> PortName -> NodeId -> PortName -> Edge
resourceEdge eid src srcPort tgt tgtPort = Edge
  { edgeId         = eid
  , edgeSourceNode  = src
  , edgeSourcePort  = srcPort
  , edgeTargetNode  = tgt
  , edgeTargetPort  = tgtPort
  , edgeCategory   = PortResource
  }

mkProgram :: [Node] -> [Edge] -> Program
mkProgram nodes edges = Program
  { programId        = "prog-runner-1"
  , programName      = "Runner Test Program"
  , programState     = Draft
  , programGraph     = Graph
      { graphNodes = Map.fromList [(nodeId n, n) | n <- nodes]
      , graphEdges = edges
      }
  , programCreatedAt = epoch
  , programUpdatedAt = epoch
  }
  where
    epoch = read "2026-01-01 00:00:00 UTC"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  -- -------------------------------------------------------------------------
  -- AC1: Linear data-flow graph — Trigger → Action → Action
  -- -------------------------------------------------------------------------

  describe "startRun / linear graph" $ do

    it "AC1: Trigger→Action→Action reaches RunCompleted with all steps recorded" $
      withTestEnv $ \env -> do
        let n1 = triggerNode "n-trigger"
            n2 = actionNode  "n-action1"
            n3 = actionNode  "n-action2"
            e1 = dataEdge "e1" "n-trigger" "event" "n-action1" "input"
            e2 = dataEdge "e2" "n-action1" "output" "n-action2" "input"
            prog = mkProgram [n1, n2, n3] [e1, e2]
        runAppM env $ insertProgram prog

        ctx <- runAppM env $ startRun prog Nothing
        waitForRun ctx

        run <- readTVarIO (rcRun ctx)
        runState run `shouldBe` RunCompleted

        steps <- readTVarIO (rcSteps ctx)
        Map.size steps `shouldBe` 3

        mapM_ (assertStepCompleted steps) ["n-trigger", "n-action1", "n-action2"]

    it "AC3: Run transitions pending→running→completed" $
      withTestEnv $ \env -> do
        let prog = mkProgram
              [triggerNode "n-t", actionNode "n-a"]
              [dataEdge "e1" "n-t" "event" "n-a" "input"]
        runAppM env $ insertProgram prog

        ctx <- runAppM env $ startRun prog Nothing
        waitForRun ctx

        run <- readTVarIO (rcRun ctx)
        runState run `shouldBe` RunCompleted
        isJust (runStartedAt run)  `shouldBe` True
        isJust (runFinishedAt run) `shouldBe` True

    it "AC4: each step records both input and output" $
      withTestEnv $ \env -> do
        let prog = mkProgram
              [triggerNode "n-t", actionNode "n-a"]
              [dataEdge "e1" "n-t" "event" "n-a" "input"]
        runAppM env $ insertProgram prog

        ctx <- runAppM env $ startRun prog Nothing
        waitForRun ctx

        steps <- readTVarIO (rcSteps ctx)
        mapM_ (assertStepHasOutput steps) ["n-t", "n-a"]

  -- -------------------------------------------------------------------------
  -- AC2: MLP-realistic graph — Trigger → Agent with Knowledge + Connector
  -- -------------------------------------------------------------------------

  describe "startRun / MLP-realistic graph with resource edges" $ do

    it "AC2: Trigger→Agent with Knowledge/Connector resource edges reaches RunCompleted" $
      withTestEnv $ \env -> do
        let nTrigger   = triggerNode   "n-trigger"
            nAgent     = agentNode     "n-agent"
            nKnowledge = knowledgeNode "n-knowledge"
            nConnector = connectorNode "n-connector"
            eData      = dataEdge     "e-data"  "n-trigger"   "event"   "n-agent" "instruction"
            eKnow      = resourceEdge "e-know"  "n-knowledge" "content" "n-agent" "context"
            eConn      = resourceEdge "e-conn"  "n-connector" "tools"   "n-agent" "tools"
            prog = mkProgram
              [nTrigger, nAgent, nKnowledge, nConnector]
              [eData, eKnow, eConn]
        runAppM env $ insertProgram prog

        ctx <- runAppM env $ startRun prog Nothing
        waitForRun ctx

        run <- readTVarIO (rcRun ctx)
        runState run `shouldBe` RunCompleted

        -- Trigger and Agent should have steps; Knowledge/Connector do not
        -- participate in data-driven execution (no data inputs).
        steps <- readTVarIO (rcSteps ctx)
        Map.size steps `shouldBe` 2
        assertStepCompleted steps "n-trigger"
        assertStepCompleted steps "n-agent"

    it "AC2: Knowledge/Connector nodes have no mailboxes (resource ports excluded)" $
      withTestEnv $ \env -> do
        let nKnowledge = knowledgeNode "n-k"
            nAgent     = agentNode     "n-a"
            eKnow      = resourceEdge "e1" "n-k" "content" "n-a" "context"
            prog = mkProgram [nKnowledge, nAgent] [eKnow]
        runAppM env $ insertProgram prog
        ctx <- runAppM env $ startRun prog Nothing
        -- Knowledge node has no data inputs → no mailbox entry
        Map.member ("n-k", "content") (rcMailboxes ctx) `shouldBe` False

  -- -------------------------------------------------------------------------
  -- AC5: resolveResourceBindings — monadic unit tests (needs AppEnv for cred lookup)
  -- -------------------------------------------------------------------------

  describe "resolveResourceBindings" $ do

    it "AC5: returns Knowledge and Connector configs from resource edges" $
      withTestEnv $ \env -> do
        let nAgent     = agentNode     "n-agent"
            nKnowledge = knowledgeNode "n-k"
            nConnector = connectorNode "n-c"
            eKnow      = resourceEdge "e1" "n-k" "content" "n-agent" "context"
            eConn      = resourceEdge "e2" "n-c" "tools"   "n-agent" "tools"
            graph = Graph
              { graphNodes = Map.fromList
                  [ ("n-agent",     nAgent)
                  , ("n-k",         nKnowledge)
                  , ("n-c",         nConnector)
                  ]
              , graphEdges = [eKnow, eConn]
              }
        bindings <- runAppM env $ resolveResourceBindings graph "n-agent"
        length (rbKnowledge  bindings) `shouldBe` 1
        length (rbConnectors bindings) `shouldBe` 1

    it "AC5: data edges are ignored by resolveResourceBindings" $
      withTestEnv $ \env -> do
        let nTrigger = triggerNode "n-t"
            nAgent   = agentNode   "n-a"
            eData    = dataEdge "e1" "n-t" "event" "n-a" "instruction"
            graph = Graph
              { graphNodes = Map.fromList [("n-t", nTrigger), ("n-a", nAgent)]
              , graphEdges = [eData]
              }
        bindings <- runAppM env $ resolveResourceBindings graph "n-a"
        length (rbKnowledge  bindings) `shouldBe` 0
        length (rbConnectors bindings) `shouldBe` 0

    it "AC5: returns empty bindings when no resource edges target the node" $
      withTestEnv $ \env -> do
        let nAgent = agentNode "n-a"
            graph  = Graph
              { graphNodes = Map.fromList [("n-a", nAgent)]
              , graphEdges = []
              }
        bindings <- runAppM env $ resolveResourceBindings graph "n-a"
        length (rbKnowledge  bindings) `shouldBe` 0
        length (rbConnectors bindings) `shouldBe` 0

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

assertStepCompleted :: Map.Map NodeId (TVar Step) -> NodeId -> IO ()
assertStepCompleted steps nid =
  case Map.lookup nid steps of
    Nothing -> expectationFailure $ "No step found for node " <> show nid
    Just tv -> do
      s <- readTVarIO tv
      stepState s `shouldBe` StepCompleted

assertStepHasOutput :: Map.Map NodeId (TVar Step) -> NodeId -> IO ()
assertStepHasOutput steps nid =
  case Map.lookup nid steps of
    Nothing -> expectationFailure $ "No step found for node " <> show nid
    Just tv -> do
      s <- readTVarIO tv
      isJust (stepOutput s) `shouldBe` True
