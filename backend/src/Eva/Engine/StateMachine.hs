{-# LANGUAGE OverloadedStrings #-}

-- | Run and Step state machines: transition allowlists, pure validation,
-- TVar-based atomic transitions, and immediate DB persistence.
-- EVA-14 (Runner) consumes these primitives to drive graph execution.
module Eva.Engine.StateMachine
  ( -- * Exception
    TransitionError (..)

    -- * Transition allowlists
  , allowedRunTransitions
  , allowedStepTransitions

    -- * Pure validation
  , validateRunTransition
  , validateStepTransition

    -- * Runtime context
  , RunContext (..)
  , newRunContext

    -- * Effectful transitions
  , transitionRun
  , transitionStep
  ) where

import Control.Concurrent.STM
import Control.Exception (Exception)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)

import Eva.App (AppM)
import Eva.Core.Types
import Eva.Persistence.Queries (updateRun, updateStepTransition)

-- ---------------------------------------------------------------------------
-- Exception
-- ---------------------------------------------------------------------------

-- | Thrown (via 'throwSTM') when a state transition is invalid.
data TransitionError = TransitionError
  { teFrom :: Text
  , teTo   :: Text
  } deriving (Show)

instance Exception TransitionError

-- ---------------------------------------------------------------------------
-- Allowlists
-- ---------------------------------------------------------------------------

-- | Valid Run state transitions. Any pair not in this set is rejected.
allowedRunTransitions :: Set (RunState, RunState)
allowedRunTransitions = Set.fromList
  [ (RunPending,  RunRunning)
  , (RunRunning,  RunCompleted)
  , (RunRunning,  RunFailed)
  , (RunRunning,  RunCanceled)
  , (RunRunning,  RunWaiting)
  , (RunWaiting,  RunRunning)
  , (RunWaiting,  RunCanceled)
  ]

-- | Valid Step state transitions. Any pair not in this set is rejected.
allowedStepTransitions :: Set (StepState, StepState)
allowedStepTransitions = Set.fromList
  [ (StepPending,  StepRunning)
  , (StepRunning,  StepCompleted)
  , (StepRunning,  StepFailed)
  , (StepRunning,  StepSkipped)
  , (StepRunning,  StepWaiting)
  , (StepWaiting,  StepRunning)
  ]

-- ---------------------------------------------------------------------------
-- Pure validation
-- ---------------------------------------------------------------------------

-- | Return 'Right ()' if the transition is allowed, 'Left' message otherwise.
validateRunTransition :: RunState -> RunState -> Either Text ()
validateRunTransition from to =
  if Set.member (from, to) allowedRunTransitions
    then Right ()
    else Left $
      "Cannot transition Run from " <> showState from <> " to " <> showState to

-- | Return 'Right ()' if the transition is allowed, 'Left' message otherwise.
validateStepTransition :: StepState -> StepState -> Either Text ()
validateStepTransition from to =
  if Set.member (from, to) allowedStepTransitions
    then Right ()
    else Left $
      "Cannot transition Step from " <> showState from <> " to " <> showState to

-- | Render a state constructor using its 'Show' instance (e.g. "RunPending").
showState :: Show a => a -> Text
showState = T.pack . show

-- ---------------------------------------------------------------------------
-- Runtime context
-- ---------------------------------------------------------------------------

-- | Per-Run execution context. EVA-14 will extend this with per-node
-- input mailboxes and a broadcast channel for WebSocket events.
data RunContext = RunContext
  { rcRun   :: TVar Run
  , rcSteps :: TVar (Map NodeId (TVar Step))
  }

-- | Allocate a fresh RunContext for the given Run.
newRunContext :: Run -> IO RunContext
newRunContext run = do
  runTVar   <- newTVarIO run
  stepsTVar <- newTVarIO Map.empty
  pure RunContext { rcRun = runTVar, rcSteps = stepsTVar }

-- ---------------------------------------------------------------------------
-- Effectful transitions
-- ---------------------------------------------------------------------------

-- | Atomically transition a Run to a new state (STM) and immediately persist
-- the change to the database. Sets 'startedAt' when entering 'running' and
-- 'finishedAt' when entering a terminal state.
--
-- Throws 'TransitionError' if the transition is not in 'allowedRunTransitions'.
transitionRun :: TVar Run -> RunState -> UTCTime -> AppM Run
transitionRun runTVar newState ts = do
  newRun <- liftIO $ atomically $ do
    run <- readTVar runTVar
    case validateRunTransition (runState run) newState of
      Left _ ->
        throwSTM (TransitionError (showState (runState run)) (showState newState))
      Right () -> do
        let updated = run
              { runState      = newState
              , runStartedAt  =
                  if newState == RunRunning
                    then Just ts
                    else runStartedAt run
              , runFinishedAt =
                  if newState `elem` [RunCompleted, RunFailed, RunCanceled]
                    then Just ts
                    else runFinishedAt run
              }
        writeTVar runTVar updated
        pure updated
  updateRun (runId newRun) (runState newRun) (runStartedAt newRun) (runFinishedAt newRun)
  pure newRun

-- | Atomically transition a Step to a new state (STM) and immediately persist
-- the change to the database. Sets 'startedAt' on the first entry into
-- 'running' and 'finishedAt' when entering a terminal state.
--
-- Throws 'TransitionError' if the transition is not in 'allowedStepTransitions'.
transitionStep :: TVar Step -> StepState -> UTCTime -> AppM Step
transitionStep stepTVar newState ts = do
  newStep <- liftIO $ atomically $ do
    step <- readTVar stepTVar
    case validateStepTransition (stepState step) newState of
      Left _ ->
        throwSTM (TransitionError (showState (stepState step)) (showState newState))
      Right () -> do
        let updated = step
              { stepState      = newState
              , stepStartedAt  =
                  if newState == StepRunning && isNothing (stepStartedAt step)
                    then Just ts
                    else stepStartedAt step
              , stepFinishedAt =
                  if newState `elem` [StepCompleted, StepFailed, StepSkipped]
                    then Just ts
                    else stepFinishedAt step
              }
        writeTVar stepTVar updated
        pure updated
  updateStepTransition
    (stepId newStep)
    (stepState newStep)
    (stepError newStep)
    (stepOutput newStep)
    (stepStartedAt newStep)
    (stepFinishedAt newStep)
  pure newStep
