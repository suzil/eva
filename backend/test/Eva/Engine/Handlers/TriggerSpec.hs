{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for EVA-25: Trigger node handler (manual event emission).
-- Tests call handleTrigger directly with a test DB containing the run.
module Eva.Engine.Handlers.TriggerSpec (spec) where

import Control.Exception (SomeException, try)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime, UTCTime)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import Eva.App (AppEnv (..), runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.Handlers.Trigger (handleTrigger)
import Eva.Engine.LLM
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries (insertProgram, insertRun)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

testProgramId :: ProgramId
testProgramId = "prog-trigger-test"

epoch :: UTCTime
epoch = read "2026-01-01 00:00:00 UTC"

testProgram :: Program
testProgram = Program
  { programId        = testProgramId
  , programName      = "Trigger Test Program"
  , programState     = Draft
  , programGraph     = Graph { graphNodes = Map.empty, graphEdges = [] }
  , programCreatedAt = epoch
  , programUpdatedAt = epoch
  }

triggerNodeWith :: NodeId -> TriggerType -> Maybe Aeson.Value -> Node
triggerNodeWith nid ttype payloadTmpl = Node
  { nodeId    = nid
  , nodeLabel = "Test Trigger"
  , nodeType  = TriggerNode TriggerConfig
      { triggerType            = ttype
      , triggerSchedule        = Nothing
      , triggerEventFilter     = Nothing
      , triggerPayloadTemplate = payloadTmpl
      }
  , nodePosX  = 0
  , nodePosY  = 0
  }

manualTriggerNode :: NodeId -> Node
manualTriggerNode nid = triggerNodeWith nid TriggerManual Nothing

testRun :: RunId -> Maybe Aeson.Value -> IO Run
testRun rid payload = do
  now <- getCurrentTime
  pure Run
    { runId          = rid
    , runProgramId   = testProgramId
    , runState       = RunRunning
    , runTriggerInfo = payload
    , runStartedAt   = Just now
    , runFinishedAt  = Nothing
    }

emptyBindings :: ResourceBindings
emptyBindings = ResourceBindings
  { rbKnowledge        = []
  , rbKnowledgeDynamic = []
  , rbConnectors       = []
  , rbConnectorRunners = []
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
      placeholderLLMClient = LLMClient
        { clientCall   = \_ -> pure (Right (LLMResponse "unused" Nothing (TokenUsage 0 0 0)))
        , clientStream = \_ _ -> pure (Right (LLMResponse "unused" Nothing (TokenUsage 0 0 0)))
        }
      env = AppEnv
        { envConfig          = cfg
        , envDbPool          = pool
        , envLogger          = \_ -> pure ()
        , envDispatch        = \_ _ _ _ -> error "dispatch not used in handler unit tests"
        , envLLMClient       = placeholderLLMClient
        , envAnthropicClient = dummyLLMClient
        , envBroadcasts      = broadcasts
        , envCredentialKey   = Crypto.deriveKey "test-key"
        }
  action env

-- | Insert program + run into DB, then run the action.
withRun :: AppEnv -> RunId -> Maybe Aeson.Value -> IO ()
         -> IO ()
withRun env rid payload cont = do
  run <- testRun rid payload
  runAppM env $ insertProgram testProgram
  runAppM env $ insertRun run
  cont

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "handleTrigger / TriggerManual" $ do

    it "emits an event message carrying the run's trigger payload" $ do
      withTestEnv $ \env -> do
        let rid     = RunId "run-trigger-1"
            nid     = NodeId "trigger-1"
            payload = Aeson.object [("key", "val")]
        withRun env rid (Just payload) $ do
          result <- runAppM env $
            handleTrigger rid (manualTriggerNode nid) Map.empty emptyBindings
          msgType    result `shouldBe` "event"
          msgPayload result `shouldBe` payload
          metaSourceNode (msgMeta result) `shouldBe` nid
          metaRunId      (msgMeta result) `shouldBe` rid

    it "uses triggerPayloadTemplate when set, ignoring run payload" $ do
      withTestEnv $ \env -> do
        let rid      = RunId "run-trigger-2"
            nid      = NodeId "trigger-2"
            tmpl     = Aeson.object [("source", "config")]
            runPayl  = Aeson.object [("source", "run")]
            node     = triggerNodeWith nid TriggerManual (Just tmpl)
        withRun env rid (Just runPayl) $ do
          result <- runAppM env $
            handleTrigger rid node Map.empty emptyBindings
          msgPayload result `shouldBe` tmpl

    it "emits an empty-object event when the run has no trigger payload" $ do
      withTestEnv $ \env -> do
        let rid = RunId "run-trigger-3"
            nid = NodeId "trigger-3"
        withRun env rid Nothing $ do
          result <- runAppM env $
            handleTrigger rid (manualTriggerNode nid) Map.empty emptyBindings
          msgType    result `shouldBe` "event"
          msgPayload result `shouldBe` Aeson.object []

    it "TriggerCron emits an event message (same as TriggerManual)" $ do
      withTestEnv $ \env -> do
        let rid  = RunId "run-trigger-4"
            nid  = NodeId "trigger-4"
            node = triggerNodeWith nid TriggerCron Nothing
        withRun env rid Nothing $ do
          result <- runAppM env $
            handleTrigger rid node Map.empty emptyBindings
          msgType result `shouldBe` "event"

    it "fails with a clear error for unimplemented trigger types (Webhook)" $ do
      withTestEnv $ \env -> do
        let rid  = RunId "run-trigger-5"
            nid  = NodeId "trigger-5"
            node = triggerNodeWith nid TriggerWebhook Nothing
        withRun env rid Nothing $ do
          result :: Either SomeException Message <-
            try $ runAppM env $
              handleTrigger rid node Map.empty emptyBindings
          case result of
            Right _  -> expectationFailure "expected exception, got success"
            Left err -> show err `shouldContain` "not yet implemented"
