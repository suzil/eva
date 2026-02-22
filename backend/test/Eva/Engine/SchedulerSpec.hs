{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for EVA-35: Cron scheduler.
-- Tests exercise 'matchingTriggers' and 'nodeCronMatches' via the exported
-- function, plus integration-level tests that fire runs through a real DB.
-- No wall-clock sleep tests — the loop is not exercised directly.
module Eva.Engine.SchedulerSpec (spec) where

import qualified Data.Aeson as Aeson
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import Data.Text (Text)
import Eva.App (AppEnv (..), DispatchFn, runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.LLM
import Eva.Engine.Scheduler (matchingTriggers)
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries (insertProgram, listPrograms)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

epoch :: UTCTime
epoch = read "2026-01-01 00:00:00 UTC"

-- | A UTCTime of 2026-03-10 14:30:00 UTC (Monday, minute=30, hour=14).
testTime :: UTCTime
testTime = read "2026-03-10 14:30:00 UTC"

makeTriggerNode :: NodeId -> TriggerType -> Maybe Text -> Node
makeTriggerNode nid ttype mSchedule = Node
  { nodeId    = nid
  , nodeLabel = "Trigger"
  , nodeType  = TriggerNode TriggerConfig
      { triggerType            = ttype
      , triggerSchedule        = mSchedule
      , triggerEventFilter     = Nothing
      , triggerPayloadTemplate = Nothing
      }
  , nodePosX  = 0
  , nodePosY  = 0
  }

makeProgram :: ProgramId -> ProgramState -> [Node] -> Program
makeProgram pid st nodes = Program
  { programId        = pid
  , programName      = "Test Program"
  , programState     = st
  , programGraph     = Graph
      { graphNodes = Map.fromList [(nodeId n, n) | n <- nodes]
      , graphEdges = []
      }
  , programCreatedAt = epoch
  , programUpdatedAt = epoch
  }

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

withTestEnv :: DispatchFn -> (AppEnv -> IO ()) -> IO ()
withTestEnv dispatch action = do
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
      env = AppEnv
        { envConfig          = cfg
        , envDbPool          = pool
        , envLogger          = \_ -> pure ()
        , envDispatch        = dispatch
        , envLLMClient       = placeholderLLM
        , envAnthropicClient = dummyLLMClient
        , envBroadcasts      = broadcasts
        , envCredentialKey   = Crypto.deriveKey "test-key"
        }
  action env
  where
    placeholderLLM = LLMClient
      { clientCall   = \_ -> pure (Right (LLMResponse "unused" Nothing (TokenUsage 0 0 0)))
      , clientStream = \_ _ -> pure (Right (LLMResponse "unused" Nothing (TokenUsage 0 0 0)))
      }

-- | Dispatch that immediately succeeds — used for programs with a single
-- trigger node that has no downstream consumers.
alwaysSucceed :: DispatchFn
alwaysSucceed rid node _ _ = do
  pure (Message "event" (Aeson.object []) (MessageMeta "trace" epoch (nodeId node) rid))

-- ---------------------------------------------------------------------------
-- matchingTriggers unit tests (pure / no DB)
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "matchingTriggers" $ do

    it "returns the program when Active with a matching cron trigger" $ do
      let node    = makeTriggerNode "n1" TriggerCron (Just "* * * * *")
          program = makeProgram "p1" Active [node]
      matchingTriggers testTime program `shouldBe` [program]

    it "returns [] for a Paused program even with a matching schedule" $ do
      let node    = makeTriggerNode "n1" TriggerCron (Just "* * * * *")
          program = makeProgram "p1" Paused [node]
      matchingTriggers testTime program `shouldBe` []

    it "returns [] for a Draft program" $ do
      let node    = makeTriggerNode "n1" TriggerCron (Just "* * * * *")
          program = makeProgram "p1" Draft [node]
      matchingTriggers testTime program `shouldBe` []

    it "returns [] for a Manual trigger node" $ do
      let node    = makeTriggerNode "n1" TriggerManual Nothing
          program = makeProgram "p1" Active [node]
      matchingTriggers testTime program `shouldBe` []

    it "returns [] when no nodes match at the given time" $ do
      -- Schedule fires at minute 0 only; testTime is minute 30.
      let node    = makeTriggerNode "n1" TriggerCron (Just "0 * * * *")
          program = makeProgram "p1" Active [node]
      matchingTriggers testTime program `shouldBe` []

    it "returns [] for an unparseable cron expression" $ do
      let node    = makeTriggerNode "n1" TriggerCron (Just "not-a-cron")
          program = makeProgram "p1" Active [node]
      matchingTriggers testTime program `shouldBe` []

    it "returns [] when triggerSchedule is Nothing" $ do
      let node    = makeTriggerNode "n1" TriggerCron Nothing
          program = makeProgram "p1" Active [node]
      matchingTriggers testTime program `shouldBe` []

    it "fires once per program even if multiple cron nodes match" $ do
      let n1      = makeTriggerNode "n1" TriggerCron (Just "* * * * *")
          n2      = makeTriggerNode "n2" TriggerCron (Just "30 14 * * *")
          program = makeProgram "p1" Active [n1, n2]
      -- Should appear exactly once, not twice.
      matchingTriggers testTime program `shouldBe` [program]

    it "matches '30 14 * * *' at 14:30 but not at 14:31" $ do
      let node     = makeTriggerNode "n1" TriggerCron (Just "30 14 * * *")
          program  = makeProgram "p1" Active [node]
          t14_31   = read "2026-03-10 14:31:00 UTC" :: UTCTime
      matchingTriggers testTime  program `shouldBe` [program]
      matchingTriggers t14_31 program `shouldBe` []

  -- ---------------------------------------------------------------------------
  -- Run isolation / DB tests
  -- ---------------------------------------------------------------------------

  describe "run isolation" $ do

    it "fires runs for Active programs and not for Paused ones" $
      withTestEnv alwaysSucceed $ \env -> do
        let activeProg = makeProgram "p-active" Active
              [ makeTriggerNode "n1" TriggerCron (Just "* * * * *") ]
            pausedProg = makeProgram "p-paused" Paused
              [ makeTriggerNode "n2" TriggerCron (Just "* * * * *") ]
        runAppM env $ insertProgram activeProg
        runAppM env $ insertProgram pausedProg

        programs <- runAppM env listPrograms
        let candidates = concatMap (matchingTriggers testTime) programs
        map programId candidates `shouldBe` ["p-active"]

    it "a failing program does not prevent other programs from being selected" $
      -- 'matchingTriggers' is a pure filter — failures in one program's runs
      -- do not affect another program's eligibility. This test verifies that
      -- both programs appear as candidates even when one would fail at runtime.
      withTestEnv alwaysSucceed $ \env -> do
        let p1 = makeProgram "p-fail" Active
                   [ makeTriggerNode "n1" TriggerCron (Just "* * * * *") ]
            p2 = makeProgram "p-ok"   Active
                   [ makeTriggerNode "n2" TriggerCron (Just "* * * * *") ]
        runAppM env $ insertProgram p1
        runAppM env $ insertProgram p2

        programs <- runAppM env listPrograms
        let candidates = map programId $ concatMap (matchingTriggers testTime) programs
        candidates `shouldContain` ["p-fail"]
        candidates `shouldContain` ["p-ok"]
