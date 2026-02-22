{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for EVA-24/EVA-33: Agent node handler.
-- Tests are isolated from the DB and graph walker — handleAgent is called
-- directly with a mock LLM client injected via 'envLLMClient'.
module Eva.Engine.Handlers.AgentSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import Eva.App (AppEnv (..), runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.Handlers.Agent (handleAgent)
import Eva.Engine.LLM
import Eva.Integration.Types
import Eva.Persistence.Migration (runMigrations)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

testRunId :: RunId
testRunId = RunId "test-run-001"

agentCfg :: AgentConfig
agentCfg = AgentConfig
  { agentModel          = "gpt-4o"
  , agentSystemPrompt   = "You are a helpful assistant."
  , agentResponseFormat = ResponseText
  , agentTemperature    = 0.7
  , agentMaxTokens      = Nothing
  , agentMaxIterations  = 1
  , agentCostBudgetUsd  = Nothing
  , agentRetryPolicy    = Nothing
  }

agentNode :: NodeId -> Node
agentNode nid = Node
  { nodeId    = nid
  , nodeLabel = "Test Agent"
  , nodeType  = AgentNode agentCfg
  , nodePosX  = 0
  , nodePosY  = 0
  }

instructionMsg :: Message
instructionMsg = Message
  { msgType    = "text"
  , msgPayload = Aeson.String "Summarise the context."
  , msgMeta    = MessageMeta
      { metaTraceId    = "trace-001"
      , metaTimestamp  = read "2026-01-01 00:00:00 UTC"
      , metaSourceNode = NodeId "trigger-1"
      , metaRunId      = testRunId
      }
  }

knowledgeCfg :: Text -> KnowledgeConfig
knowledgeCfg content = KnowledgeConfig
  { knowledgeSource        = InlineText content
  , knowledgeFormat        = FormatText
  , knowledgeRefreshPolicy = RefreshStatic
  }

emptyBindings :: ResourceBindings
emptyBindings = ResourceBindings
  { rbKnowledge        = []
  , rbKnowledgeDynamic = []
  , rbConnectors       = []
  , rbConnectorRunners = []
  }

-- ---------------------------------------------------------------------------
-- Mock LLM clients
-- ---------------------------------------------------------------------------

-- | Returns a fixed response text.
mockLLMClient :: Text -> LLMClient
mockLLMClient response = LLMClient
  { clientCall   = \_ -> pure (Right (LLMResponse response Nothing (TokenUsage 10 5 15)))
  , clientStream = \_ _ -> pure (Right (LLMResponse response Nothing (TokenUsage 10 5 15)))
  }

-- | Captures the last request received and returns a fixed response.
capturingLLMClient :: IORef (Maybe LLMRequest) -> Text -> LLMClient
capturingLLMClient ref response = LLMClient
  { clientCall   = \r -> do
      writeIORef ref (Just r)
      pure (Right (LLMResponse response Nothing (TokenUsage 10 5 15)))
  , clientStream = \r _ -> do
      writeIORef ref (Just r)
      pure (Right (LLMResponse response Nothing (TokenUsage 10 5 15)))
  }

-- | Always returns an auth error.
failingLLMClient :: LLMClient
failingLLMClient = LLMClient
  { clientCall   = \_ -> pure (Left (LLMAuthError "invalid api key"))
  , clientStream = \_ _ -> pure (Left (LLMAuthError "invalid api key"))
  }

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

withTestEnv :: LLMClient -> (AppEnv -> IO ()) -> IO ()
withTestEnv llmClient action = do
  pool       <- runNoLoggingT $ createSqlitePool ":memory:" 2
  runMigrations pool
  broadcasts <- newTVarIO Map.empty
  let cfg = AppConfig
        { configDbPath        = ":memory:"
        , configPort          = 8080
        , configLlmApiKey     = Nothing
        , configLogLevel      = LogError
        , configCredentialKey = "test-key"
        }
      env = AppEnv
        { envConfig        = cfg
        , envDbPool        = pool
        , envLogger        = \_ -> pure ()
        , envDispatch      = \_ _ _ _ -> error "dispatch not used in handler unit tests"
        , envLLMClient     = llmClient
        , envBroadcasts    = broadcasts
        , envCredentialKey = Crypto.deriveKey "test-key"
        }
  action env

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "handleAgent" $ do

    it "returns an agent_output message when the LLM succeeds" $ do
      withTestEnv (mockLLMClient "The answer is 42.") $ \env -> do
        let nid    = NodeId "agent-1"
            inputs = Map.fromList [("instruction", instructionMsg)]
        result <- runAppM env $ handleAgent testRunId (agentNode nid) inputs emptyBindings
        msgType    result `shouldBe` "agent_output"
        msgPayload result `shouldBe` Aeson.String "The answer is 42."
        metaSourceNode (msgMeta result) `shouldBe` nid
        metaRunId      (msgMeta result) `shouldBe` testRunId

    it "includes Knowledge context in the user message" $ do
      ref <- newIORef Nothing
      withTestEnv (capturingLLMClient ref "ok") $ \env -> do
        let nid      = NodeId "agent-2"
            inputs   = Map.fromList [("instruction", instructionMsg)]
            bindings = ResourceBindings
              { rbKnowledge        = [knowledgeCfg "Team goal: ship Eva by June."]
              , rbKnowledgeDynamic = []
              , rbConnectors       = []
              , rbConnectorRunners = []
              }
        _ <- runAppM env $ handleAgent testRunId (agentNode nid) inputs bindings
        mReq <- readIORef ref
        case mReq of
          Nothing  -> expectationFailure "LLM client was never called"
          Just req -> do
            let userMsg = last (llmMessages req)
            T.unpack (chatContent userMsg) `shouldContain` "Summarise the context."
            T.unpack (chatContent userMsg) `shouldContain` "Team goal: ship Eva by June."

    it "includes multiple Knowledge nodes separated by a divider" $ do
      ref <- newIORef Nothing
      withTestEnv (capturingLLMClient ref "ok") $ \env -> do
        let nid      = NodeId "agent-3"
            inputs   = Map.fromList [("instruction", instructionMsg)]
            bindings = ResourceBindings
              { rbKnowledge        = [ knowledgeCfg "Fact A"
                                     , knowledgeCfg "Fact B"
                                     ]
              , rbKnowledgeDynamic = []
              , rbConnectors       = []
              , rbConnectorRunners = []
              }
        _ <- runAppM env $ handleAgent testRunId (agentNode nid) inputs bindings
        mReq <- readIORef ref
        case mReq of
          Nothing  -> expectationFailure "LLM client was never called"
          Just req -> do
            let userMsg = last (llmMessages req)
            T.unpack (chatContent userMsg) `shouldContain` "Fact A"
            T.unpack (chatContent userMsg) `shouldContain` "Fact B"
            T.unpack (chatContent userMsg) `shouldContain` "---"

    it "sends the system prompt as the first chat message" $ do
      ref <- newIORef Nothing
      withTestEnv (capturingLLMClient ref "ok") $ \env -> do
        let inputs = Map.fromList [("instruction", instructionMsg)]
        _ <- runAppM env $
          handleAgent testRunId (agentNode (NodeId "agent-4")) inputs emptyBindings
        mReq <- readIORef ref
        case mReq of
          Nothing  -> expectationFailure "LLM client was never called"
          Just req -> do
            let sysMsg = head (llmMessages req)
            T.unpack (chatRole    sysMsg) `shouldBe` "system"
            T.unpack (chatContent sysMsg) `shouldBe` "You are a helpful assistant."

    it "fails with a clear error when 'instruction' input is missing" $ do
      withTestEnv (mockLLMClient "ignored") $ \env -> do
        let inputs = Map.empty
        result :: Either SomeException Message <-
          try $ runAppM env $
            handleAgent testRunId (agentNode (NodeId "agent-5")) inputs emptyBindings
        case result of
          Right _  -> expectationFailure "expected exception, got success"
          Left err -> show err `shouldContain` "instruction"

    it "fails when the LLM client returns an error" $ do
      withTestEnv failingLLMClient $ \env -> do
        let inputs = Map.fromList [("instruction", instructionMsg)]
        result :: Either SomeException Message <-
          try $ runAppM env $
            handleAgent testRunId (agentNode (NodeId "agent-6")) inputs emptyBindings
        case result of
          Right _  -> expectationFailure "expected exception, got success"
          Left err -> show err `shouldContain` "invalid api key"

    it "ignores non-inline Knowledge sources (FileRef/UrlRef) in rbKnowledge" $ do
      ref <- newIORef Nothing
      withTestEnv (capturingLLMClient ref "ok") $ \env -> do
        let inputs   = Map.fromList [("instruction", instructionMsg)]
            bindings = ResourceBindings
              { rbKnowledge        = [ KnowledgeConfig (FileRef "/some/file.txt")      FormatText RefreshStatic
                                     , KnowledgeConfig (UrlRef  "https://example.com") FormatText RefreshStatic
                                     , KnowledgeConfig (InlineText "Only this")        FormatText RefreshStatic
                                     ]
              , rbKnowledgeDynamic = []
              , rbConnectors       = []
              , rbConnectorRunners = []
              }
        _ <- runAppM env $
          handleAgent testRunId (agentNode (NodeId "agent-7")) inputs bindings
        mReq <- readIORef ref
        case mReq of
          Nothing  -> expectationFailure "LLM client was never called"
          Just req -> do
            let userMsg = last (llmMessages req)
            T.unpack (chatContent userMsg) `shouldContain`    "Only this"
            T.unpack (chatContent userMsg) `shouldNotContain` "/some/file.txt"
            T.unpack (chatContent userMsg) `shouldNotContain` "https://example.com"

  -- -------------------------------------------------------------------------
  -- Tool-call loop (EVA-33)
  -- -------------------------------------------------------------------------

  describe "tool-call loop" $ do

    it "executes a tool call and produces final output" $ do
      -- Round 1: LLM returns tool_calls; round 2: LLM returns text.
      -- maxIterations=5 so the loop has room for the tool-call round + final response.
      callCountRef <- newIORef (0 :: Int)
      let toolCallResp = LLMResponse
            { llmContent   = ""
            , llmToolCalls = Just [ToolCall "call-1" "list_issues" (Aeson.object [])]
            , llmUsage     = TokenUsage 10 0 10
            }
          finalResp = LLMResponse
            { llmContent   = "Here are the issues: issue-1, issue-2"
            , llmToolCalls = Nothing
            , llmUsage     = TokenUsage 20 10 30
            }
          sequentialClient = LLMClient
            { clientCall   = \_ -> do
                n <- readIORef callCountRef
                modifyIORef' callCountRef (+1)
                pure $ Right $ if n == 0 then toolCallResp else finalResp
            , clientStream = \_ _ -> pure (Right finalResp)
            }
          mockRunner = ConnectorRunner
            { connectorAvailableActions = pure
                [ ActionSpec "list_issues" "List Linear issues" (Aeson.object []) "json" ]
            , connectorExecuteAction = \_ _ ->
                pure (Right (Aeson.String "issue-1, issue-2"))
            }
          multiTurnCfg = agentCfg { agentMaxIterations = 5 }
          bindings = emptyBindings { rbConnectorRunners = [mockRunner] }
          inputs   = Map.fromList [("instruction", instructionMsg)]
      withTestEnv sequentialClient $ \env -> do
        result <- runAppM env $
          handleAgent testRunId
            (agentNode (NodeId "tool-agent-1")) { nodeType = AgentNode multiTurnCfg }
            inputs bindings
        msgType    result `shouldBe` "agent_output"
        msgPayload result `shouldBe` Aeson.String "Here are the issues: issue-1, issue-2"
        -- LLM should have been called twice (tool-call round + final round)
        readIORef callCountRef >>= (`shouldBe` 2)

    it "includes tool results in the follow-up LLM request" $ do
      -- Capture the second LLM request to verify tool result messages were appended.
      callCountRef  <- newIORef (0 :: Int)
      capturedMsgs  <- newIORef ([] :: [ChatMessage])
      let toolCallResp n = LLMResponse
              { llmContent   = ""
              , llmToolCalls = if n == 0
                  then Just [ToolCall "call-1" "list_issues" (Aeson.object [])]
                  else Nothing
              , llmUsage     = TokenUsage 5 0 5
              }
          finalRespText = LLMResponse "done" Nothing (TokenUsage 5 5 10)
          trackingClient = LLMClient
            { clientCall   = \req -> do
                n <- readIORef callCountRef
                modifyIORef' callCountRef (+1)
                when (n == 1) $ writeIORef capturedMsgs (llmMessages req)
                pure $ Right $ if n == 0 then toolCallResp 0 else finalRespText
            , clientStream = \_ _ -> pure (Right finalRespText)
            }
          mockRunner = ConnectorRunner
            { connectorAvailableActions = pure
                [ ActionSpec "list_issues" "List issues" (Aeson.object []) "json" ]
            , connectorExecuteAction = \_ _ ->
                pure (Right (Aeson.String "EVA-1: build Eva"))
            }
          multiTurnCfg = agentCfg { agentMaxIterations = 5 }
          bindings = emptyBindings { rbConnectorRunners = [mockRunner] }
          inputs   = Map.fromList [("instruction", instructionMsg)]
      withTestEnv trackingClient $ \env -> do
        _ <- runAppM env $
          handleAgent testRunId
            (agentNode (NodeId "tool-agent-2")) { nodeType = AgentNode multiTurnCfg }
            inputs bindings
        msgs <- readIORef capturedMsgs
        -- The second request must contain a ToolCallMsg followed by a ToolResultMsg
        let hasToolCallMsg   = any isToolCallMsg  msgs
            hasToolResultMsg = any isToolResultMsg msgs
        hasToolCallMsg   `shouldBe` True
        hasToolResultMsg `shouldBe` True

    it "raises an error when max_iterations is reached" $ do
      -- LLM always returns tool_calls; maxIterations = 2.
      let alwaysToolCall = LLMClient
            { clientCall   = \_ -> pure $ Right $ LLMResponse
                { llmContent   = ""
                , llmToolCalls = Just [ToolCall "call-n" "list_issues" (Aeson.object [])]
                , llmUsage     = TokenUsage 5 0 5
                }
            , clientStream = \_ _ -> pure (Right (LLMResponse "ok" Nothing (TokenUsage 1 1 2)))
            }
          mockRunner = ConnectorRunner
            { connectorAvailableActions = pure
                [ ActionSpec "list_issues" "List issues" (Aeson.object []) "json" ]
            , connectorExecuteAction = \_ _ ->
                pure (Right (Aeson.String "result"))
            }
          limitedCfg = agentCfg { agentMaxIterations = 2 }
          bindings   = emptyBindings { rbConnectorRunners = [mockRunner] }
          inputs     = Map.fromList [("instruction", instructionMsg)]
      withTestEnv alwaysToolCall $ \env -> do
        result :: Either SomeException Message <-
          try $ runAppM env $
            handleAgent testRunId
              (agentNode (NodeId "tool-agent-3")) { nodeType = AgentNode limitedCfg }
              inputs bindings
        case result of
          Right _  -> expectationFailure "expected max_iterations error"
          Left err -> show err `shouldContain` "max_iterations"

    it "stops when cost budget is exceeded" $ do
      -- Very small budget; first tool-call response burns it.
      callCountRef <- newIORef (0 :: Int)
      let expensiveResp = LLMResponse
              { llmContent   = ""
              , llmToolCalls = Just [ToolCall "call-x" "list_issues" (Aeson.object [])]
              , llmUsage     = TokenUsage 1_000_000 1_000_000 2_000_000  -- huge token count
              }
          cheapFinalResp = LLMResponse "final" Nothing (TokenUsage 1 1 2)
          costClient = LLMClient
            { clientCall   = \_ -> do
                n <- readIORef callCountRef
                modifyIORef' callCountRef (+1)
                pure $ Right $ if n == 0 then expensiveResp else cheapFinalResp
            , clientStream = \_ _ -> pure (Right cheapFinalResp)
            }
          mockRunner = ConnectorRunner
            { connectorAvailableActions = pure
                [ ActionSpec "list_issues" "List issues" (Aeson.object []) "json" ]
            , connectorExecuteAction = \_ _ ->
                pure (Right (Aeson.String "done"))
            }
          tightBudgetCfg = agentCfg
            { agentCostBudgetUsd  = Just 0.001  -- $0.001 — exhausted by 2M tokens
            , agentMaxIterations  = 10
            }
          bindings = emptyBindings { rbConnectorRunners = [mockRunner] }
          inputs   = Map.fromList [("instruction", instructionMsg)]
      withTestEnv costClient $ \env -> do
        result <- runAppM env $
          handleAgent testRunId
            (agentNode (NodeId "tool-agent-4")) { nodeType = AgentNode tightBudgetCfg }
            inputs bindings
        -- Should return with budget-exceeded message, not throw
        msgType result `shouldBe` "agent_output"
        T.unpack (extractPayloadText (msgPayload result)) `shouldContain` "cost budget exceeded"

    it "uses clientStream (not clientCall) when no connectors are wired" $ do
      streamCalledRef <- newIORef False
      callCalledRef   <- newIORef False
      let trackingClient = LLMClient
            { clientCall   = \_ -> do
                writeIORef callCalledRef True
                pure (Right (LLMResponse "from call" Nothing (TokenUsage 1 1 2)))
            , clientStream = \_ _ -> do
                writeIORef streamCalledRef True
                pure (Right (LLMResponse "from stream" Nothing (TokenUsage 1 1 2)))
            }
          inputs = Map.fromList [("instruction", instructionMsg)]
      withTestEnv trackingClient $ \env -> do
        result <- runAppM env $
          handleAgent testRunId (agentNode (NodeId "tool-agent-5")) inputs emptyBindings
        readIORef streamCalledRef >>= (`shouldBe` True)
        readIORef callCalledRef   >>= (`shouldBe` False)
        msgPayload result `shouldBe` Aeson.String "from stream"

-- ---------------------------------------------------------------------------
-- Helpers for tool-call message inspection
-- ---------------------------------------------------------------------------

isToolCallMsg :: ChatMessage -> Bool
isToolCallMsg (ToolCallMsg _) = True
isToolCallMsg _               = False

isToolResultMsg :: ChatMessage -> Bool
isToolResultMsg (ToolResultMsg _ _) = True
isToolResultMsg _                   = False

extractPayloadText :: Value -> Text
extractPayloadText (Aeson.String t) = t
extractPayloadText v                = T.pack (show v)
