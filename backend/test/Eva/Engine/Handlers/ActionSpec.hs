{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for EVA-25: Action node handler (template substitution).
-- Tests call handleAction directly — no DB or graph walker involved.
module Eva.Engine.Handlers.ActionSpec (spec) where

import Control.Exception (SomeException, try)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import Eva.App (AppEnv (..), runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import Eva.Core.Types
import Eva.Engine.Handlers.Action (handleAction)
import Eva.Engine.LLM
import Eva.Persistence.Migration (runMigrations)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

testRunId :: RunId
testRunId = RunId "test-run-action-001"

actionNodeWith :: NodeId -> ActionOperation -> Aeson.Value -> Node
actionNodeWith nid op params = Node
  { nodeId    = nid
  , nodeLabel = "Test Action"
  , nodeType  = ActionNode ActionConfig
      { actionOperation     = op
      , actionParameters    = params
      , actionErrorHandling = ErrFail
      , actionRetryPolicy   = Nothing
      }
  , nodePosX  = 0
  , nodePosY  = 0
  }

templateNode :: NodeId -> Text -> Node
templateNode nid tmpl =
  actionNodeWith nid OpTemplate
    (Aeson.object [("template", Aeson.String tmpl)])

inputMsg :: Aeson.Value -> Message
inputMsg payload = Message
  { msgType    = "event"
  , msgPayload = payload
  , msgMeta    = MessageMeta
      { metaTraceId    = "trace-001"
      , metaTimestamp  = read "2026-01-01 00:00:00 UTC"
      , metaSourceNode = NodeId "trigger-1"
      , metaRunId      = testRunId
      }
  }

emptyBindings :: ResourceBindings
emptyBindings = ResourceBindings { rbKnowledge = [], rbConnectors = [] }

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

withTestEnv :: (AppEnv -> IO ()) -> IO ()
withTestEnv action = do
  pool       <- runNoLoggingT $ createSqlitePool ":memory:" 2
  runMigrations pool
  broadcasts <- newTVarIO Map.empty
  let cfg = AppConfig
        { configDbPath    = ":memory:"
        , configPort      = 8080
        , configLlmApiKey = Nothing
        , configLogLevel  = LogError
        }
      dummyLLMClient = LLMClient
        { clientCall   = \_ -> pure (Right (LLMResponse "unused" (TokenUsage 0 0 0)))
        , clientStream = \_ _ -> pure (Right (LLMResponse "unused" (TokenUsage 0 0 0)))
        }
      env = AppEnv
        { envConfig     = cfg
        , envDbPool     = pool
        , envLogger     = \_ -> pure ()
        , envDispatch   = \_ _ _ _ -> error "dispatch not used in handler unit tests"
        , envLLMClient  = dummyLLMClient
        , envBroadcasts = broadcasts
        }
  action env

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "handleAction / OpTemplate" $ do

    it "substitutes a single variable" $ do
      withTestEnv $ \env -> do
        let nid    = NodeId "action-1"
            node   = templateNode nid "Hello {{name}}"
            inputs = Map.fromList
              [("input", inputMsg (Aeson.object [("name", "World")]))]
        result <- runAppM env $ handleAction testRunId node inputs emptyBindings
        msgType    result `shouldBe` "action_output"
        msgPayload result `shouldBe` Aeson.String "Hello World"
        metaSourceNode (msgMeta result) `shouldBe` nid
        metaRunId      (msgMeta result) `shouldBe` testRunId

    it "substitutes multiple variables" $ do
      withTestEnv $ \env -> do
        let node   = templateNode (NodeId "action-2") "{{greeting}} {{name}}"
            inputs = Map.fromList
              [("input", inputMsg (Aeson.object
                  [ ("greeting", "Hello")
                  , ("name",     "Alice")
                  ]))]
        result <- runAppM env $ handleAction testRunId node inputs emptyBindings
        msgPayload result `shouldBe` Aeson.String "Hello Alice"

    it "passes through a template with no variables unchanged" $ do
      withTestEnv $ \env -> do
        let node   = templateNode (NodeId "action-3") "static output"
            inputs = Map.fromList [("input", inputMsg (Aeson.object []))]
        result <- runAppM env $ handleAction testRunId node inputs emptyBindings
        msgPayload result `shouldBe` Aeson.String "static output"

    it "fails with a descriptive error when a variable is missing" $ do
      withTestEnv $ \env -> do
        let node   = templateNode (NodeId "action-4") "Hello {{name}}"
            inputs = Map.fromList
              [("input", inputMsg (Aeson.object [("other", "value")]))]
        result :: Either SomeException Message <-
          try $ runAppM env $ handleAction testRunId node inputs emptyBindings
        case result of
          Right _  -> expectationFailure "expected exception, got success"
          Left err -> show err `shouldContain` "name"

    it "fails with a clear error for non-template operations" $ do
      withTestEnv $ \env -> do
        let node   = actionNodeWith (NodeId "action-5") OpCode
                       (Aeson.object [("code", "print('hi')")])
            inputs = Map.fromList [("input", inputMsg (Aeson.object []))]
        result :: Either SomeException Message <-
          try $ runAppM env $ handleAction testRunId node inputs emptyBindings
        case result of
          Right _  -> expectationFailure "expected exception, got success"
          Left err -> show err `shouldContain` "not implemented"
