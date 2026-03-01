{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for EVA-85: MAGI assistant LLM dispatch + conversation loop.
-- Tests are isolated from WebSocket wiring — handleAssistantMessage is called
-- directly with a mock LLM client. The DB is real (in-memory SQLite) so that
-- tool handlers that query persistence (get_graph, search_programs) work correctly.
module Eva.AssistantSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sqlite (createSqlitePool)
import Control.Monad.Logger (runNoLoggingT)
import Test.Hspec

import Eva.App (AppEnv (..), runAppM)
import Eva.Assistant
  ( AssistantContext (..)
  , AssistantMessage (..)
  , ConversationId (..)
  , handleAssistantMessage
  )
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.LLM
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries (insertProgram)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

emptyContext :: AssistantContext
emptyContext = AssistantContext
  { ctxProgramId    = Nothing
  , ctxProgramName  = Nothing
  , ctxProgramState = Nothing
  , ctxGraphSummary = Nothing
  , ctxSelectedNode = Nothing
  , ctxCurrentMode  = "author"
  , ctxActiveRunId  = Nothing
  , ctxRecentErrors = []
  , ctxProgramList  = []
  }

-- | A minimal valid graph: one Trigger node, no edges.
-- Passes all validateGraph checks.
validMinimalGraph :: Graph
validMinimalGraph = Graph
  { graphNodes = Map.fromList
      [ ( NodeId "t1"
        , Node
            { nodeId    = NodeId "t1"
            , nodeLabel = "Start"
            , nodeType  = TriggerNode TriggerConfig
                { triggerType            = TriggerManual
                , triggerSchedule        = Nothing
                , triggerEventFilter     = Nothing
                , triggerPayloadTemplate = Nothing
                }
            , nodePosX  = 0
            , nodePosY  = 0
            }
        )
      ]
  , graphEdges = []
  }

-- | An invalid graph: one Agent node, no Trigger.
-- Fails validateGraph with "must contain at least one Trigger node".
noTriggerGraph :: Graph
noTriggerGraph = Graph
  { graphNodes = Map.fromList
      [ ( NodeId "a1"
        , Node
            { nodeId    = NodeId "a1"
            , nodeLabel = "Agent"
            , nodeType  = AgentNode AgentConfig
                { agentProvider       = Nothing
                , agentModel          = "gpt-4o"
                , agentSystemPrompt   = "Be helpful."
                , agentResponseFormat = ResponseText
                , agentTemperature    = 0.7
                , agentMaxTokens              = Nothing
                , agentMaxIterations          = 3
                , agentCostBudgetUsd          = Nothing
                , agentRetryPolicy            = Nothing
                , agentPromptVariableBindings = Nothing
                }
            , nodePosX  = 0
            , nodePosY  = 0
            }
        )
      ]
  , graphEdges = []
  }

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

withTestEnv :: LLMClient -> (AppEnv -> IO ()) -> IO ()
withTestEnv llmClient action = do
  pool       <- runNoLoggingT $ createSqlitePool ":memory:" 2
  runMigrations pool
  broadcasts <- newTVarIO Map.empty
  cancelTokens <- newTVarIO Map.empty
  let cfg = AppConfig
        { configDbPath          = ":memory:"
        , configPort            = 8080
        , configLlmApiKey       = Nothing
        , configAnthropicApiKey = Nothing
        , configLogLevel        = LogError
        , configCredentialKey   = "test-key"
        , configStaticDir       = Nothing
        }
      env = AppEnv
        { envConfig          = cfg
        , envDbPool          = pool
        , envLogger          = \_ -> pure ()
        , envDispatch        = \_ _ _ _ -> error "dispatch not used in assistant tests"
        , envLLMClient       = llmClient
        , envAnthropicClient = dummyLLMClient
        , envBroadcasts      = broadcasts
        , envCredentialKey   = Crypto.deriveKey "test-key"
        , envCancelTokens    = cancelTokens
        }
  action env

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "handleAssistantMessage" $ do

    -- AC: A /generate message produces a graph_proposal AssistantMessage
    -- with a valid Graph payload.
    it "propose_graph with valid graph returns AsstGraphProposal" $ do
      let expectedSummary = "A simple trigger program." :: Text
          proposalResp    = LLMResponse
            { llmContent   = ""
            , llmToolCalls = Just
                [ ToolCall "call-1" "propose_graph" $ Aeson.object
                    [ "graph"   .= validMinimalGraph
                    , "summary" .= expectedSummary
                    ]
                ]
            , llmUsage = TokenUsage 10 0 10
            }
          mockClient = LLMClient
            { clientCall   = \_ -> pure (Right proposalResp)
            , clientStream = \_ _ -> pure (Right proposalResp)
            }
      withTestEnv mockClient $ \env -> do
        result <- runAppM env $
          handleAssistantMessage
            (ConversationId "conv-1")
            "/generate a trigger program"
            emptyContext
            (\_ -> pure ())
        case result of
          AsstGraphProposal g summary -> do
            -- Graph payload contains the trigger node from the proposal
            Map.member (NodeId "t1") (graphNodes g) `shouldBe` True
            summary `shouldBe` expectedSummary
          other -> expectationFailure $
            "expected AsstGraphProposal, got: " <> show other

    -- AC: Tool calls to get_graph return the actual current program graph
    -- (not a mock); the tool queries the real in-memory DB.
    it "get_graph returns the actual program graph from the DB" $ do
      callCountRef <- newIORef (0 :: Int)
      let testPid    = "prog-get-graph-test" :: Text
          stateClient = LLMClient
            { clientCall   = \_ -> getGraphThenText callCountRef testPid
            , clientStream = \_ _ -> getGraphThenText callCountRef testPid
            }
      withTestEnv stateClient $ \env -> do
        let epoch = posixSecondsToUTCTime 0
        runAppM env $ insertProgram Program
          { programId        = ProgramId testPid
          , programName      = "Graph Retrieval Test"
          , programState     = Draft
          , programGraph     = validMinimalGraph
          , programCreatedAt = epoch
          , programUpdatedAt = epoch
          }
        result <- runAppM env $
          handleAssistantMessage
            (ConversationId "conv-2")
            "What nodes are in this program?"
            emptyContext
            (\_ -> pure ())
        case result of
          AsstText t -> T.unpack t `shouldContain` "Graph retrieved."
          other      -> expectationFailure $
            "expected AsstText after get_graph loop, got: " <> show other
        -- LLM was called exactly twice: tool call round then text round
        readIORef callCountRef >>= (`shouldBe` 2)

    -- AC: propose_graph with an invalid graph (no Trigger) does NOT produce
    -- AsstGraphProposal; instead validation errors are fed back to the LLM.
    it "propose_graph with invalid graph feeds validation errors back to LLM" $ do
      callCountRef <- newIORef (0 :: Int)
      let invalidProposalResp = LLMResponse
            { llmContent   = ""
            , llmToolCalls = Just
                [ ToolCall "call-2" "propose_graph" $ Aeson.object
                    [ "graph"   .= noTriggerGraph
                    , "summary" .= ("An agent-only program." :: Text)
                    ]
                ]
            , llmUsage = TokenUsage 10 0 10
            }
          correctionResp = LLMResponse
            { llmContent   = "I will add a Trigger node and resubmit."
            , llmToolCalls = Nothing
            , llmUsage     = TokenUsage 10 10 20
            }
          statefulClient = LLMClient
            { clientCall = \_ -> do
                n <- readIORef callCountRef
                modifyIORef' callCountRef (+1)
                pure $ Right $ if n == 0 then invalidProposalResp else correctionResp
            , clientStream = \_ onToken -> do
                n <- readIORef callCountRef
                modifyIORef' callCountRef (+1)
                if n == 0
                  then pure $ Right invalidProposalResp
                  else do
                    onToken "I will add a Trigger node and resubmit."
                    pure $ Right correctionResp
            }
      withTestEnv statefulClient $ \env -> do
        result <- runAppM env $
          handleAssistantMessage
            (ConversationId "conv-3")
            "/generate"
            emptyContext
            (\_ -> pure ())
        -- Must NOT produce a proposal from a broken graph
        case result of
          AsstGraphProposal _ _ -> expectationFailure
            "must not produce AsstGraphProposal for a graph that fails validation"
          AsstText t ->
            -- LLM acknowledged the validation error
            T.unpack t `shouldContain` "Trigger"
          _ -> pure ()
        -- LLM called twice: invalid proposal received errors, then returned text
        readIORef callCountRef >>= (`shouldBe` 2)

    -- AC: Max 8 tool calls per message is enforced; a truncation AsstText
    -- is returned and the LLM is not called a ninth time.
    it "enforces a maximum of 8 tool call iterations" $ do
      callCountRef <- newIORef (0 :: Int)
      let infiniteToolCallClient = LLMClient
            { clientCall   = \_ -> do
                modifyIORef' callCountRef (+1)
                pure $ Right $ LLMResponse ""
                  (Just [ToolCall "c" "search_programs"
                          (Aeson.object ["query" .= ("eva" :: Text)])])
                  (TokenUsage 5 0 5)
            , clientStream = \_ _ -> do
                modifyIORef' callCountRef (+1)
                pure $ Right $ LLMResponse ""
                  (Just [ToolCall "c" "search_programs"
                          (Aeson.object ["query" .= ("eva" :: Text)])])
                  (TokenUsage 5 0 5)
            }
      withTestEnv infiniteToolCallClient $ \env -> do
        result <- runAppM env $
          handleAssistantMessage
            (ConversationId "conv-4")
            "search everything"
            emptyContext
            (\_ -> pure ())
        -- Guard fires before iteration 9; result is the depth-limit message
        case result of
          AsstText t -> T.unpack t `shouldContain` "depth limit"
          other      -> expectationFailure $
            "expected truncation AsstText, got: " <> show other
        -- LLM called exactly 8 times (iterations 0 through 7)
        readIORef callCountRef >>= (`shouldBe` 8)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Two-round client: first call returns a get_graph tool call, second
-- returns text. Shared between clientCall and clientStream branches.
getGraphThenText :: IORef Int -> Text -> IO (Either LLMError LLMResponse)
getGraphThenText countRef pid = do
  n <- readIORef countRef
  modifyIORef' countRef (+1)
  if n == 0
    then pure $ Right $ LLMResponse ""
           (Just [ToolCall "c1" "get_graph" (Aeson.object ["programId" .= pid])])
           (TokenUsage 5 0 5)
    else pure $ Right $ LLMResponse "Graph retrieved." Nothing (TokenUsage 5 5 10)
