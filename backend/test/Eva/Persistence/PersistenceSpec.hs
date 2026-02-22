{-# LANGUAGE OverloadedStrings #-}

module Eva.Persistence.PersistenceSpec (spec) where

import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sql (selectList)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import Eva.App (AppEnv (..), AppM, runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import Eva.Core.Types
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries
import Eva.Persistence.Schema

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

-- | Create an in-memory SQLite pool, run migrations, and return an AppEnv
-- suitable for running AppM actions in tests.
withTestEnv :: (AppEnv -> IO ()) -> IO ()
withTestEnv action = do
  pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
  runMigrations pool
  let cfg = AppConfig
        { configDbPath    = ":memory:"
        , configPort      = 8080
        , configLlmApiKey = Nothing
        , configLogLevel  = LogError
        }
  let env = AppEnv
        { envConfig = cfg
        , envDbPool = pool
        , envLogger = \_ -> pure ()
        }
  action env

-- | Run an AppM action against the test environment.
runTest :: AppEnv -> AppM a -> IO a
runTest = runAppM

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

t0 :: UTCTime
t0 = posixSecondsToUTCTime 1_740_000_000

t1 :: UTCTime
t1 = posixSecondsToUTCTime 1_740_001_000

sampleProgram :: Program
sampleProgram =
  Program
    { programId        = "prog-test-1"
    , programName      = "Test Program"
    , programState     = Draft
    , programGraph     = Graph
        { graphNodes = Map.fromList
            [ ( "n-agent"
              , Node
                  { nodeId    = "n-agent"
                  , nodeLabel = "My Agent"
                  , nodeType  = AgentNode AgentConfig
                      { agentModel          = "gpt-4o"
                      , agentSystemPrompt   = "You are helpful."
                      , agentResponseFormat = ResponseText
                      , agentTemperature    = 0.7
                      , agentMaxTokens      = Just 1024
                      , agentMaxIterations  = 3
                      , agentCostBudgetUsd  = Nothing
                      }
                  , nodePosX  = 100
                  , nodePosY  = 200
                  }
              )
            , ( "n-trigger"
              , Node
                  { nodeId    = "n-trigger"
                  , nodeLabel = "Start"
                  , nodeType  = TriggerNode TriggerConfig
                      { triggerType            = TriggerManual
                      , triggerSchedule        = Nothing
                      , triggerEventFilter     = Nothing
                      , triggerPayloadTemplate = Nothing
                      }
                  , nodePosX  = 0
                  , nodePosY  = 200
                  }
              )
            ]
        , graphEdges =
            [ Edge
                { edgeId         = "e-1"
                , edgeSourceNode = "n-trigger"
                , edgeSourcePort = "event"
                , edgeTargetNode = "n-agent"
                , edgeTargetPort = "instruction"
                , edgeCategory   = PortData
                }
            ]
        }
    , programCreatedAt = t0
    , programUpdatedAt = t0
    }

-- | A program containing all 5 MLP NodeType variants.
allNodeTypesProgram :: Program
allNodeTypesProgram =
  Program
    { programId    = "prog-all-types"
    , programName  = "All Node Types"
    , programState = Draft
    , programGraph = Graph
        { graphNodes = Map.fromList
            [ mk "n-agent"
                (AgentNode AgentConfig
                  { agentModel = "gpt-4o", agentSystemPrompt = "sys"
                  , agentResponseFormat = ResponseText, agentTemperature = 0.5
                  , agentMaxTokens = Just 512, agentMaxIterations = 1
                  , agentCostBudgetUsd = Just 1.0
                  })
            , mk "n-knowledge"
                (KnowledgeNode KnowledgeConfig
                  { knowledgeSource        = InlineText "facts"
                  , knowledgeFormat        = FormatText
                  , knowledgeRefreshPolicy = RefreshStatic
                  })
            , mk "n-connector"
                (ConnectorNode ConnectorConfig
                  { connectorSystem       = SystemLinear
                  , connectorCredentialId = Just "cred-1"
                  , connectorEndpoint     = Nothing
                  , connectorScope        = Nothing
                  , connectorActionFilter = []
                  })
            , mk "n-action"
                (ActionNode ActionConfig
                  { actionOperation   = OpTemplate
                  , actionParameters  = "{}"
                  , actionErrorHandling = ErrFail
                  })
            , mk "n-trigger"
                (TriggerNode TriggerConfig
                  { triggerType            = TriggerCron
                  , triggerSchedule        = Just "0 9 * * 1"
                  , triggerEventFilter     = Nothing
                  , triggerPayloadTemplate = Nothing
                  })
            ]
        , graphEdges = []
        }
    , programCreatedAt = t0
    , programUpdatedAt = t0
    }
  where
    mk nid nt =
      let (NodeId t) = nid
      in ( nid
         , Node { nodeId = nid, nodeLabel = t, nodeType = nt
                , nodePosX = 0, nodePosY = 0 }
         )

sampleRun :: Run
sampleRun =
  Run
    { runId          = "run-1"
    , runProgramId   = "prog-test-1"
    , runState       = RunRunning
    , runTriggerInfo = Nothing
    , runStartedAt   = Just t0
    , runFinishedAt  = Nothing
    }

sampleStep :: Step
sampleStep =
  Step
    { stepId         = "step-1"
    , stepRunId      = "run-1"
    , stepNodeId     = "n-agent"
    , stepState      = StepRunning
    , stepInput      = Nothing
    , stepOutput     = Nothing
    , stepError      = Nothing
    , stepRetryCount = 0
    , stepStartedAt  = Just t0
    , stepFinishedAt = Nothing
    }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = around withTestEnv $ do

  describe "migration" $ do
    it "creates all 7 tables; re-running is idempotent" $ \env -> do
      -- Running migrations a second time should not throw
      runMigrations (envDbPool env)
      -- Verify tables exist by running a simple query against each
      runTest env $ runDb $ do
        _ <- selectList @ProgramRow [] []
        _ <- selectList @NodeRow [] []
        _ <- selectList @EdgeRow [] []
        _ <- selectList @RunRow [] []
        _ <- selectList @StepRow [] []
        _ <- selectList @LogEntryRow [] []
        _ <- selectList @CredentialRow [] []
        pure ()

  describe "program round-trip" $ do
    it "insert then getProgram returns the same program" $ \env -> do
      runTest env (insertProgram sampleProgram)
      result <- runTest env (getProgram "prog-test-1")
      result `shouldBe` Just sampleProgram

    it "getProgram returns Nothing for unknown id" $ \env -> do
      result <- runTest env (getProgram "no-such-program")
      result `shouldBe` Nothing

    it "listPrograms returns all inserted programs" $ \env -> do
      runTest env (insertProgram sampleProgram)
      programs <- runTest env listPrograms
      map programId programs `shouldBe` ["prog-test-1"]

    it "updateProgram changes name and state" $ \env -> do
      runTest env (insertProgram sampleProgram)
      let updated = sampleProgram { programName = "Renamed", programState = Active, programUpdatedAt = t1 }
      runTest env (updateProgram updated)
      result <- runTest env (getProgram "prog-test-1")
      fmap programName result  `shouldBe` Just "Renamed"
      fmap programState result `shouldBe` Just Active

    it "deleteProgram removes the program and its nodes/edges" $ \env -> do
      runTest env (insertProgram sampleProgram)
      runTest env (deleteProgram "prog-test-1")
      result <- runTest env (getProgram "prog-test-1")
      result `shouldBe` Nothing

  describe "putGraph" $ do
    it "replaces the graph: old nodes gone, new nodes present" $ \env -> do
      runTest env (insertProgram sampleProgram)
      let newNode = Node
            { nodeId    = "n-new"
            , nodeLabel = "New Node"
            , nodeType  = TriggerNode TriggerConfig
                { triggerType = TriggerManual, triggerSchedule = Nothing
                , triggerEventFilter = Nothing, triggerPayloadTemplate = Nothing
                }
            , nodePosX  = 50
            , nodePosY  = 50
            }
          newGraph = Graph
            { graphNodes = Map.fromList [("n-new", newNode)]
            , graphEdges = []
            }
      runTest env (putGraph "prog-test-1" newGraph)
      result <- runTest env (getProgram "prog-test-1")
      case result of
        Nothing -> expectationFailure "program not found after putGraph"
        Just p  -> do
          Map.keys (graphNodes (programGraph p)) `shouldBe` ["n-new"]
          graphEdges (programGraph p) `shouldBe` []

  describe "all 5 NodeType configs round-trip" $ do
    it "each NodeType variant survives encode -> persist -> decode" $ \env -> do
      runTest env (insertProgram allNodeTypesProgram)
      result <- runTest env (getProgram "prog-all-types")
      case result of
        Nothing -> expectationFailure "allNodeTypesProgram not found"
        Just p  ->
          graphNodes (programGraph p) `shouldBe` graphNodes (programGraph allNodeTypesProgram)

  describe "run round-trip" $ do
    it "insertRun then getRun returns the same run" $ \env -> do
      runTest env (insertProgram sampleProgram)
      runTest env (insertRun sampleRun)
      result <- runTest env (getRun "run-1")
      result `shouldBe` Just sampleRun

    it "listRunsForProgram returns runs for that program" $ \env -> do
      runTest env (insertProgram sampleProgram)
      runTest env (insertRun sampleRun)
      runs <- runTest env (listRunsForProgram "prog-test-1" 100 0)
      map runId runs `shouldBe` ["run-1"]

    it "updateRun changes state and timestamps" $ \env -> do
      runTest env (insertProgram sampleProgram)
      runTest env (insertRun sampleRun)
      runTest env (updateRun "run-1" RunCompleted (Just t0) (Just t1))
      result <- runTest env (getRun "run-1")
      fmap runState     result `shouldBe` Just RunCompleted
      fmap runFinishedAt result `shouldBe` Just (Just t1)

  describe "step round-trip" $ do
    it "insertStep then getStep returns the same step" $ \env -> do
      runTest env (insertProgram sampleProgram)
      runTest env (insertRun sampleRun)
      runTest env (insertStep sampleStep)
      result <- runTest env (getStep "step-1")
      result `shouldBe` Just sampleStep

    it "listStepsForRun returns steps for that run" $ \env -> do
      runTest env (insertProgram sampleProgram)
      runTest env (insertRun sampleRun)
      runTest env (insertStep sampleStep)
      steps <- runTest env (listStepsForRun "run-1")
      map stepId steps `shouldBe` ["step-1"]
