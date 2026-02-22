{-# LANGUAGE OverloadedStrings #-}

module Eva.Engine.StateMachineSpec (spec) where

import Control.Concurrent.Async (async, waitCatch)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Exception (SomeException)
import Control.Monad.Logger (runNoLoggingT)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import Eva.App (AppEnv (..), AppM, runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.Dispatch (execute)
import Eva.Engine.LLM (dummyLLMClient)
import Eva.Engine.StateMachine
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

withTestEnv :: (AppEnv -> IO ()) -> IO ()
withTestEnv action = do
  pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
  runMigrations pool
  let cfg = AppConfig
        { configDbPath        = ":memory:"
        , configPort          = 8080
        , configLlmApiKey       = Nothing
        , configAnthropicApiKey = Nothing
        , configLogLevel        = LogError
        , configCredentialKey   = "test-key"
        , configStaticDir       = Nothing
        }
  broadcasts <- newTVarIO Map.empty
  let env = AppEnv
        { envConfig          = cfg
        , envDbPool          = pool
        , envLogger          = \_ -> pure ()
        , envDispatch        = execute
        , envLLMClient       = dummyLLMClient
        , envAnthropicClient = dummyLLMClient
        , envBroadcasts      = broadcasts
        , envCredentialKey   = Crypto.deriveKey "test-key"
        }
  action env

runTest :: AppEnv -> AppM a -> IO a
runTest = runAppM

-- ---------------------------------------------------------------------------
-- Timestamps
-- ---------------------------------------------------------------------------

t0 :: UTCTime
t0 = posixSecondsToUTCTime 1_740_000_000

t1 :: UTCTime
t1 = posixSecondsToUTCTime 1_740_001_000

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | Minimal program required to satisfy the runs.program_id FK.
smProgram :: Program
smProgram = Program
  { programId        = "prog-sm-1"
  , programName      = "SM Test Program"
  , programState     = Draft
  , programGraph     = Graph { graphNodes = Map.empty, graphEdges = [] }
  , programCreatedAt = t0
  , programUpdatedAt = t0
  }

pendingRun :: Run
pendingRun = Run
  { runId          = "run-sm-1"
  , runProgramId   = "prog-sm-1"
  , runState       = RunPending
  , runTriggerInfo = Nothing
  , runStartedAt   = Nothing
  , runFinishedAt  = Nothing
  }

pendingStep :: Step
pendingStep = Step
  { stepId         = "step-sm-1"
  , stepRunId      = "run-sm-1"
  , stepNodeId     = "n-agent"
  , stepState      = StepPending
  , stepInput      = Nothing
  , stepOutput     = Nothing
  , stepError      = Nothing
  , stepRetryCount = 0
  , stepStartedAt  = Nothing
  , stepFinishedAt = Nothing
  }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  -- -------------------------------------------------------------------------
  -- Pure: Run transitions
  -- -------------------------------------------------------------------------

  describe "validateRunTransition" $ do

    it "accepts every pair in allowedRunTransitions" $ do
      let results = [validateRunTransition f t | (f, t) <- Set.toList allowedRunTransitions]
      all (== Right ()) results `shouldBe` True

    it "rejects a transition not in the allowlist" $ do
      validateRunTransition RunCompleted RunRunning
        `shouldBe` Left "Cannot transition Run from RunCompleted to RunRunning"

    it "rejects self-transition" $ do
      validateRunTransition RunRunning RunRunning
        `shouldBe` Left "Cannot transition Run from RunRunning to RunRunning"

    it "rejects all outgoing transitions from terminal states" $ do
      let terminalStates = [RunCompleted, RunFailed, RunCanceled]
          allStates      = [minBound .. maxBound] :: [RunState]
          results        = [ validateRunTransition from to
                           | from <- terminalStates, to <- allStates ]
      all (/= Right ()) results `shouldBe` True

    it "error message mentions both states" $ do
      case validateRunTransition RunPending RunCompleted of
        Right ()  -> expectationFailure "Expected Left but got Right ()"
        Left  msg -> do
          T.unpack msg `shouldContain` "RunPending"
          T.unpack msg `shouldContain` "RunCompleted"

  -- -------------------------------------------------------------------------
  -- Pure: Step transitions
  -- -------------------------------------------------------------------------

  describe "validateStepTransition" $ do

    it "accepts every pair in allowedStepTransitions" $ do
      let results = [validateStepTransition f t | (f, t) <- Set.toList allowedStepTransitions]
      all (== Right ()) results `shouldBe` True

    it "rejects a transition not in the allowlist" $ do
      validateStepTransition StepCompleted StepRunning
        `shouldBe` Left "Cannot transition Step from StepCompleted to StepRunning"

    it "rejects self-transition" $ do
      validateStepTransition StepRunning StepRunning
        `shouldBe` Left "Cannot transition Step from StepRunning to StepRunning"

    it "rejects all outgoing transitions from terminal states" $ do
      let terminalStates = [StepCompleted, StepFailed, StepSkipped]
          allStates      = [minBound .. maxBound] :: [StepState]
          results        = [ validateStepTransition from to
                           | from <- terminalStates, to <- allStates ]
      all (/= Right ()) results `shouldBe` True

    it "error message mentions both states" $ do
      case validateStepTransition StepPending StepCompleted of
        Right ()  -> expectationFailure "Expected Left but got Right ()"
        Left  msg -> do
          T.unpack msg `shouldContain` "StepPending"
          T.unpack msg `shouldContain` "StepCompleted"

  -- -------------------------------------------------------------------------
  -- Effectful: transitionRun
  -- -------------------------------------------------------------------------

  describe "transitionRun" $ do

    it "pending→running: updates TVar state and sets startedAt" $
      withTestEnv $ \env -> do
        runTest env $ insertProgram smProgram
        runTest env $ insertRun pendingRun
        runTVar <- newTVarIO pendingRun
        result  <- runTest env $ transitionRun runTVar RunRunning t0
        runState result      `shouldBe` RunRunning
        runStartedAt result  `shouldBe` Just t0
        runFinishedAt result `shouldBe` Nothing
        -- TVar reflects new state
        inMem <- readTVarIO runTVar
        runState inMem `shouldBe` RunRunning

    it "pending→running: persists to DB" $
      withTestEnv $ \env -> do
        runTest env $ insertProgram smProgram
        runTest env $ insertRun pendingRun
        runTVar <- newTVarIO pendingRun
        _       <- runTest env $ transitionRun runTVar RunRunning t0
        mDbRun  <- runTest env $ getRun "run-sm-1"
        case mDbRun of
          Nothing    -> expectationFailure "Run not found in DB"
          Just dbRun -> do
            runState dbRun     `shouldBe` RunRunning
            runStartedAt dbRun `shouldBe` Just t0

    it "running→completed: sets finishedAt and persists" $
      withTestEnv $ \env -> do
        let startedRun = pendingRun { runState = RunRunning, runStartedAt = Just t0 }
        runTest env $ insertProgram smProgram
        runTest env $ insertRun startedRun
        runTVar <- newTVarIO startedRun
        result  <- runTest env $ transitionRun runTVar RunCompleted t1
        runState result      `shouldBe` RunCompleted
        runFinishedAt result `shouldBe` Just t1
        mDbRun <- runTest env $ getRun "run-sm-1"
        fmap runFinishedAt mDbRun `shouldBe` Just (Just t1)

    it "throws TransitionError on invalid transition" $
      withTestEnv $ \env -> do
        runTest env $ insertProgram smProgram
        runTest env $ insertRun pendingRun
        runTVar <- newTVarIO pendingRun
        let action = runTest env $ transitionRun runTVar RunCompleted t0
        action `shouldThrow` (\(TransitionError f t) -> f == "RunPending" && t == "RunCompleted")

    it "does not change TVar when transition is invalid" $
      withTestEnv $ \env -> do
        runTest env $ insertProgram smProgram
        runTest env $ insertRun pendingRun
        runTVar <- newTVarIO pendingRun
        _       <- runTest env (transitionRun runTVar RunCompleted t0) `shouldThrow` anyException
        inMem   <- readTVarIO runTVar
        runState inMem `shouldBe` RunPending

  -- -------------------------------------------------------------------------
  -- Effectful: transitionStep
  -- -------------------------------------------------------------------------

  describe "transitionStep" $ do

    it "pending→running: sets startedAt" $
      withTestEnv $ \env -> do
        runTest env $ insertProgram smProgram
        runTest env $ insertRun pendingRun
        runTest env $ insertStep pendingStep
        stepTVar <- newTVarIO pendingStep
        result   <- runTest env $ transitionStep stepTVar StepRunning t0
        stepState result     `shouldBe` StepRunning
        stepStartedAt result `shouldBe` Just t0
        stepFinishedAt result `shouldBe` Nothing

    it "pending→running: persists startedAt to DB" $
      withTestEnv $ \env -> do
        runTest env $ insertProgram smProgram
        runTest env $ insertRun pendingRun
        runTest env $ insertStep pendingStep
        stepTVar <- newTVarIO pendingStep
        _        <- runTest env $ transitionStep stepTVar StepRunning t0
        mDbStep  <- runTest env $ getStep "step-sm-1"
        case mDbStep of
          Nothing     -> expectationFailure "Step not found in DB"
          Just dbStep -> stepStartedAt dbStep `shouldBe` Just t0

    it "running→completed: sets finishedAt" $
      withTestEnv $ \env -> do
        let startedStep = pendingStep { stepState = StepRunning, stepStartedAt = Just t0 }
        runTest env $ insertProgram smProgram
        runTest env $ insertRun pendingRun
        runTest env $ insertStep startedStep
        stepTVar <- newTVarIO startedStep
        result   <- runTest env $ transitionStep stepTVar StepCompleted t1
        stepState result      `shouldBe` StepCompleted
        stepStartedAt result  `shouldBe` Just t0
        stepFinishedAt result `shouldBe` Just t1

    it "waiting→running: preserves existing startedAt" $
      withTestEnv $ \env -> do
        let waitingStep = pendingStep
              { stepState     = StepWaiting
              , stepStartedAt = Just t0
              }
        runTest env $ insertProgram smProgram
        runTest env $ insertRun pendingRun
        runTest env $ insertStep waitingStep
        stepTVar <- newTVarIO waitingStep
        result   <- runTest env $ transitionStep stepTVar StepRunning t1
        stepStartedAt result `shouldBe` Just t0

    it "throws TransitionError on invalid transition" $
      withTestEnv $ \env -> do
        runTest env $ insertProgram smProgram
        runTest env $ insertRun pendingRun
        runTest env $ insertStep pendingStep
        stepTVar <- newTVarIO pendingStep
        let action = runTest env $ transitionStep stepTVar StepCompleted t0
        action `shouldThrow` (\(TransitionError f t) -> f == "StepPending" && t == "StepCompleted")

  -- -------------------------------------------------------------------------
  -- Effectful: STM atomicity (concurrent transitions)
  -- -------------------------------------------------------------------------

  describe "STM atomicity" $ do

    it "concurrent pending→running: exactly one succeeds, one gets TransitionError" $
      withTestEnv $ \env -> do
        runTest env $ insertProgram smProgram
        runTest env $ insertRun pendingRun
        runTVar <- newTVarIO pendingRun
        a1 <- async $ runTest env $ transitionRun runTVar RunRunning t0
        a2 <- async $ runTest env $ transitionRun runTVar RunRunning t0
        r1 <- waitCatch a1 :: IO (Either SomeException Run)
        r2 <- waitCatch a2 :: IO (Either SomeException Run)
        let (failures, successes) = partitionEithers [r1, r2]
        length successes `shouldBe` 1
        length failures  `shouldBe` 1
        -- Final TVar state is running
        inMem <- readTVarIO runTVar
        runState inMem `shouldBe` RunRunning
