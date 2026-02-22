{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for EVA-24: Agent node handler.
-- Tests are isolated from the DB and graph walker — handleAgent is called
-- directly with a mock LLM client injected via 'envLLMClient'.
module Eva.Engine.Handlers.AgentSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, try)
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
  { clientCall   = \_ -> pure (Right (LLMResponse response (TokenUsage 10 5 15)))
  , clientStream = \_ _ -> pure (Right (LLMResponse response (TokenUsage 10 5 15)))
  }

-- | Captures the last request received and returns a fixed response.
capturingLLMClient :: IORef (Maybe LLMRequest) -> Text -> LLMClient
capturingLLMClient ref response = LLMClient
  { clientCall   = \r -> do
      writeIORef ref (Just r)
      pure (Right (LLMResponse response (TokenUsage 10 5 15)))
  , clientStream = \r _ -> do
      writeIORef ref (Just r)
      pure (Right (LLMResponse response (TokenUsage 10 5 15)))
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
