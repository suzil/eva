{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for EVA-29: Knowledge node handler.
-- Tests are isolated from the DB and graph walker — handleKnowledge is called
-- directly with fixture nodes and input maps.
module Eva.Engine.Handlers.KnowledgeSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import Eva.App (AppEnv (..), runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.Handlers.Knowledge (handleKnowledge)
import Eva.Engine.LLM (LLMClient (..), dummyLLMClient)
import Eva.Persistence.Migration (runMigrations)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

testRunId :: RunId
testRunId = RunId "test-run-knowledge-001"

mkNode :: NodeId -> KnowledgeConfig -> Node
mkNode nid cfg = Node
  { nodeId    = nid
  , nodeLabel = "Test Knowledge"
  , nodeType  = KnowledgeNode cfg
  , nodePosX  = 0
  , nodePosY  = 0
  }

inlineCfg :: KnowledgeFormat -> Text -> KnowledgeConfig
inlineCfg fmt content = KnowledgeConfig
  { knowledgeSource        = InlineText content
  , knowledgeFormat        = fmt
  , knowledgeRefreshPolicy = RefreshStatic
  }

upstreamCfg :: KnowledgeConfig
upstreamCfg = KnowledgeConfig
  { knowledgeSource        = UpstreamPort
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

-- | Build an "update" input message carrying the given payload.
updateMsg :: Value -> Message
updateMsg payload = Message
  { msgType    = "update"
  , msgPayload = payload
  , msgMeta    = MessageMeta
      { metaTraceId    = "trace-k-001"
      , metaTimestamp  = read "2026-01-01 00:00:00 UTC"
      , metaSourceNode = NodeId "upstream-1"
      , metaRunId      = testRunId
      }
  }

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

withTestEnv :: (AppEnv -> IO ()) -> IO ()
withTestEnv action = do
  pool       <- runNoLoggingT $ createSqlitePool ":memory:" 2
  runMigrations pool
  broadcasts <- newTVarIO Map.empty
  let cfg = AppConfig
        { configDbPath        = ":memory:"
        , configPort          = 8080
        , configLlmApiKey       = Nothing
        , configAnthropicApiKey = Nothing
        , configLogLevel        = LogError
        , configCredentialKey   = "test-key"
        }
      dummyLLM = LLMClient
        { clientCall   = \_ -> error "LLM not used in Knowledge handler tests"
        , clientStream = \_ _ -> error "LLM not used in Knowledge handler tests"
        }
      env = AppEnv
        { envConfig          = cfg
        , envDbPool          = pool
        , envLogger          = \_ -> pure ()
        , envDispatch        = \_ _ _ _ -> error "dispatch not used in handler unit tests"
        , envLLMClient       = dummyLLM
        , envAnthropicClient = dummyLLMClient
        , envBroadcasts      = broadcasts
        , envCredentialKey   = Crypto.deriveKey "test-key"
        }
  action env

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "handleKnowledge" $ do

    it "returns a knowledge_content message for InlineText (FormatText)" $
      withTestEnv $ \env -> do
        let nid  = NodeId "k-1"
            node = mkNode nid (inlineCfg FormatText "Team goals: ship by June.")
        result <- runAppM env $
          handleKnowledge testRunId node Map.empty emptyBindings
        msgType    result `shouldBe` "knowledge_content"
        msgPayload result `shouldBe` Aeson.String "Team goals: ship by June."
        metaSourceNode (msgMeta result) `shouldBe` nid
        metaRunId      (msgMeta result) `shouldBe` testRunId

    it "encodes FormatText content as a bare JSON string" $
      withTestEnv $ \env -> do
        let node = mkNode (NodeId "k-2") (inlineCfg FormatText "plain text")
        result <- runAppM env $
          handleKnowledge testRunId node Map.empty emptyBindings
        msgPayload result `shouldBe` Aeson.String "plain text"

    it "encodes FormatEmbedded content as a bare JSON string" $
      withTestEnv $ \env -> do
        let node = mkNode (NodeId "k-2b") (inlineCfg FormatEmbedded "embed me")
        result <- runAppM env $
          handleKnowledge testRunId node Map.empty emptyBindings
        msgPayload result `shouldBe` Aeson.String "embed me"

    it "parses FormatJson content as a JSON object when valid JSON" $
      withTestEnv $ \env -> do
        let node = mkNode (NodeId "k-3")
                     (inlineCfg FormatJson "{\"key\":\"value\"}")
        result <- runAppM env $
          handleKnowledge testRunId node Map.empty emptyBindings
        msgPayload result `shouldBe` Aeson.object [("key", Aeson.String "value")]

    it "falls back to a JSON string for FormatJson when content is not valid JSON" $
      withTestEnv $ \env -> do
        let node = mkNode (NodeId "k-4") (inlineCfg FormatJson "not json at all")
        result <- runAppM env $
          handleKnowledge testRunId node Map.empty emptyBindings
        msgPayload result `shouldBe` Aeson.String "not json at all"

    it "returns the update input payload for UpstreamPort source" $
      withTestEnv $ \env -> do
        let nid     = NodeId "k-5"
            node    = mkNode nid upstreamCfg
            payload = Aeson.String "Dynamic sprint summary."
            inputs  = Map.fromList [("update", updateMsg payload)]
        result <- runAppM env $
          handleKnowledge testRunId node inputs emptyBindings
        msgType    result `shouldBe` "knowledge_content"
        msgPayload result `shouldBe` payload
        metaSourceNode (msgMeta result) `shouldBe` nid

    it "propagates a JSON object payload from the update port unchanged" $
      withTestEnv $ \env -> do
        let node    = mkNode (NodeId "k-6") upstreamCfg
            payload = Aeson.object [("issues", Aeson.Number 5)]
            inputs  = Map.fromList [("update", updateMsg payload)]
        result <- runAppM env $
          handleKnowledge testRunId node inputs emptyBindings
        msgPayload result `shouldBe` payload

    it "throws when UpstreamPort source has no update input" $
      withTestEnv $ \env -> do
        let node = mkNode (NodeId "k-7") upstreamCfg
        result :: Either SomeException Message <-
          try $ runAppM env $
            handleKnowledge testRunId node Map.empty emptyBindings
        case result of
          Right _  -> expectationFailure "expected exception, got success"
          Left err -> show err `shouldContain` "update"

    it "throws for FileRef source (not implemented in M5)" $
      withTestEnv $ \env -> do
        let cfg  = KnowledgeConfig (FileRef "/data/context.txt") FormatText RefreshStatic
            node = mkNode (NodeId "k-8") cfg
        result :: Either SomeException Message <-
          try $ runAppM env $
            handleKnowledge testRunId node Map.empty emptyBindings
        case result of
          Right _  -> expectationFailure "expected exception, got success"
          Left err -> show err `shouldContain` "not implemented"

    it "throws for UrlRef source (not implemented in M5)" $
      withTestEnv $ \env -> do
        let cfg  = KnowledgeConfig (UrlRef "https://example.com/context") FormatText RefreshStatic
            node = mkNode (NodeId "k-9") cfg
        result :: Either SomeException Message <-
          try $ runAppM env $
            handleKnowledge testRunId node Map.empty emptyBindings
        case result of
          Right _  -> expectationFailure "expected exception, got success"
          Left err -> show err `shouldContain` "not implemented"
