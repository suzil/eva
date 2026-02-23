{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for EVA-29 + EVA-46: Knowledge node handler.
-- Tests are isolated from the DB and graph walker — handleKnowledge is called
-- directly with fixture nodes and input maps.
module Eva.Engine.Handlers.KnowledgeSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, bracket, try)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sqlite (createSqlitePool)
import Network.HTTP.Types (hContentType, status200, status404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Directory (removeFile)
import System.IO (hClose, hPutStr, openTempFile)
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
        , configStaticDir       = Nothing
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
-- FileRef helpers
-- ---------------------------------------------------------------------------

-- | Create a temporary file with the given content, run the action with its
-- path, then delete the file.
withTempKnowledgeFile :: String -> (FilePath -> IO a) -> IO a
withTempKnowledgeFile content action =
  bracket
    (do (path, h) <- openTempFile "/tmp" "eva-test-knowledge"
        hPutStr h content
        hClose h
        pure path)
    removeFile
    action

-- ---------------------------------------------------------------------------
-- UrlRef mock server helpers
-- ---------------------------------------------------------------------------

-- | Spin up a temporary WAI server on a free port, run the action with the
-- port number, then shut it down.
withMockServer :: Wai.Application -> (Int -> IO a) -> IO a
withMockServer app = Warp.testWithApplication (pure app)

plainTextApp :: Text -> Wai.Application
plainTextApp body _req respond =
  respond $ Wai.responseLBS status200
    [(hContentType, "text/plain; charset=utf-8")]
    (BSLC.pack (T.unpack body))

htmlApp :: Text -> Wai.Application
htmlApp body _req respond =
  respond $ Wai.responseLBS status200
    [(hContentType, "text/html; charset=utf-8")]
    (BSLC.pack (T.unpack body))

notFoundApp :: Wai.Application
notFoundApp _req respond =
  respond $ Wai.responseLBS status404 [] "Not Found"

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

    it "reads file content for a valid FileRef path" $
      withTempKnowledgeFile "Team goals: ship Eva by June." $ \path ->
        withTestEnv $ \env -> do
          let cfg  = KnowledgeConfig (FileRef (T.pack path)) FormatText RefreshStatic
              node = mkNode (NodeId "k-8") cfg
          result <- runAppM env $
            handleKnowledge testRunId node Map.empty emptyBindings
          msgType    result `shouldBe` "knowledge_content"
          msgPayload result `shouldBe` Aeson.String "Team goals: ship Eva by June."

    it "returns an error message for a missing FileRef path" $
      withTestEnv $ \env -> do
        let cfg  = KnowledgeConfig (FileRef "/tmp/eva-no-such-file-xyz.txt") FormatText RefreshStatic
            node = mkNode (NodeId "k-9") cfg
        result :: Either SomeException Message <-
          try $ runAppM env $
            handleKnowledge testRunId node Map.empty emptyBindings
        case result of
          Right _  -> expectationFailure "expected exception, got success"
          Left err -> show err `shouldContain` "not found"

    it "returns plain-text HTTP body for a valid UrlRef (200)" $
      withMockServer (plainTextApp "Sprint summary: 3 issues closed.") $ \port ->
        withTestEnv $ \env -> do
          let url  = "http://localhost:" <> T.pack (show port) <> "/"
              cfg  = KnowledgeConfig (UrlRef url) FormatText RefreshStatic
              node = mkNode (NodeId "k-10") cfg
          result <- runAppM env $
            handleKnowledge testRunId node Map.empty emptyBindings
          msgType    result `shouldBe` "knowledge_content"
          msgPayload result `shouldBe` Aeson.String "Sprint summary: 3 issues closed."

    it "returns HTTP status code in the error for a non-200 UrlRef" $
      withMockServer notFoundApp $ \port ->
        withTestEnv $ \env -> do
          let url  = "http://localhost:" <> T.pack (show port) <> "/"
              cfg  = KnowledgeConfig (UrlRef url) FormatText RefreshStatic
              node = mkNode (NodeId "k-11") cfg
          result :: Either SomeException Message <-
            try $ runAppM env $
              handleKnowledge testRunId node Map.empty emptyBindings
          case result of
            Right _  -> expectationFailure "expected exception, got success"
            Left err -> show err `shouldContain` "404"

    it "strips HTML tags for a text/html UrlRef response" $
      withMockServer (htmlApp "<html><body><h1>Hello</h1><p>World</p></body></html>") $ \port ->
        withTestEnv $ \env -> do
          let url  = "http://localhost:" <> T.pack (show port) <> "/"
              cfg  = KnowledgeConfig (UrlRef url) FormatText RefreshStatic
              node = mkNode (NodeId "k-12") cfg
          result <- runAppM env $
            handleKnowledge testRunId node Map.empty emptyBindings
          case msgPayload result of
            Aeson.String txt -> do
              T.unpack txt `shouldContain` "Hello"
              T.unpack txt `shouldContain` "World"
            other -> expectationFailure $ "expected String payload, got: " <> show other
