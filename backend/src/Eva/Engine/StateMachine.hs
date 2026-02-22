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
  , waitForRun
  , markUnhandledError

    -- * Effectful transitions
  , transitionRun
  , transitionStep
  ) where

import Control.Concurrent.STM
import Control.Exception (Exception)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
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
  , (StepPending,  StepSkipped)   -- for skipDescendants: unreachable nodes
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

-- | Per-Run execution context. Holds all mutable state for one graph execution.
data RunContext = RunContext
  { rcRun               :: TVar Run
  , rcRunId             :: RunId
    -- ^ Immutable convenience copy of the RunId (avoids STM reads for logging/meta).
  , rcSteps             :: TVar (Map NodeId (TVar Step))
  , rcMailboxes         :: Map (NodeId, PortName) (TMVar Message)
    -- ^ One empty TMVar per data-input port. Resource ports are excluded.
  , rcDispatched        :: TVar (Set NodeId)
    -- ^ Nodes whose async worker has already been forked (prevents double-dispatch).
  , rcAllDone           :: TVar Bool
    -- ^ Set to True by the walker when all terminal nodes have completed/failed.
  , rcHasUnhandledError :: TVar Bool
    -- ^ Set to True when a step fails with no wired error port. Causes the
    -- Run to transition to Failed rather than Completed in 'finishRun'.
  , rcBroadcast         :: TChan Value
    -- ^ Broadcast channel for WebSocket events (consumed by EVA-26).
  }

-- | Allocate a fresh RunContext for the given Run.
-- @dataPorts@ lists every (NodeId, PortName) that should receive a data mailbox;
-- typically all (nodeId, port) pairs from 'Eva.Core.Graph.requiredDataInputs'.
newRunContext :: Run -> [(NodeId, PortName)] -> IO RunContext
newRunContext run dataPorts = do
  runTVar       <- newTVarIO run
  stepsTVar     <- newTVarIO Map.empty
  mailboxes     <- Map.fromList <$> mapM (\k -> (k,) <$> newEmptyTMVarIO) dataPorts
  dispatched    <- newTVarIO Set.empty
  allDone       <- newTVarIO False
  unhandledErr  <- newTVarIO False
  broadcast     <- newBroadcastTChanIO
  pure RunContext
    { rcRun               = runTVar
    , rcRunId             = runId run
    , rcSteps             = stepsTVar
    , rcMailboxes         = mailboxes
    , rcDispatched        = dispatched
    , rcAllDone           = allDone
    , rcHasUnhandledError = unhandledErr
    , rcBroadcast         = broadcast
    }

-- | Record that a branch has failed without a wired error port.
-- The Run will transition to Failed rather than Completed when done.
markUnhandledError :: RunContext -> IO ()
markUnhandledError ctx = atomically $ writeTVar (rcHasUnhandledError ctx) True

-- | Block until the Run has reached a terminal state (Completed, Failed, or Canceled).
-- This is safe to call from any thread and guarantees the Run record is fully
-- persisted before returning.
waitForRun :: RunContext -> IO ()
waitForRun ctx = atomically $ do
  run <- readTVar (rcRun ctx)
  let isTerminal = runState run `elem` [RunCompleted, RunFailed, RunCanceled]
  if isTerminal then pure () else retry

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
