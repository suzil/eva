{-# LANGUAGE OverloadedStrings #-}

-- | In-process cron scheduler: fires TriggerCron nodes on schedule.
-- Wakes at each minute boundary, evaluates cron expressions against the
-- current time, and calls 'startRun' for every matching Active program.
-- Each fired run is tracked as an 'Async ()' for graceful shutdown.
module Eva.Engine.Scheduler
  ( SchedulerHandle
  , startScheduler
  , shutdownScheduler
    -- * Exposed for testing
  , matchingTriggers
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.Maybe (isNothing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
  ( UTCTime
  , getCurrentTime
  , utctDayTime
  )
import System.Cron (parseCronSchedule, scheduleMatches)

import Eva.App (AppEnv, logMsg, runAppM)
import Eva.Config (LogLevel (..))
import Eva.Core.Types
import Eva.Engine.Runner (startRun, waitForRun)
import Eva.Persistence.Queries (listPrograms)

-- ---------------------------------------------------------------------------
-- Handle
-- ---------------------------------------------------------------------------

-- | Opaque handle returned by 'startScheduler'. Pass to 'shutdownScheduler'
-- for graceful shutdown: stops firing new runs, waits up to 30 s for all
-- in-flight runs to complete, then cancels the background loop.
data SchedulerHandle = SchedulerHandle
  { shShutdown :: TVar Bool
  , shLoop     :: Async ()
  , shInflight :: TVar (Map Int (Async ()))
  , shNextId   :: TVar Int
  }

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Fork the cron loop in the background and return a handle for shutdown.
startScheduler :: AppEnv -> IO SchedulerHandle
startScheduler env = do
  shutdown  <- newTVarIO False
  inflightV <- newTVarIO Map.empty
  nextIdV   <- newTVarIO (0 :: Int)
  loopA     <- async $ schedulerLoop env shutdown inflightV nextIdV
  runAppM env $ logMsg LogInfo "Cron scheduler started"
  pure SchedulerHandle
    { shShutdown = shutdown
    , shLoop     = loopA
    , shInflight = inflightV
    , shNextId   = nextIdV
    }

-- | Graceful shutdown: stop firing new runs, wait up to 30 s for in-flight
-- runs to complete, then cancel the loop async.
shutdownScheduler :: SchedulerHandle -> IO ()
shutdownScheduler h = do
  atomically $ writeTVar (shShutdown h) True
  waitInflight (shInflight h) 30
  cancel (shLoop h)

-- | Poll inflight asyncs every 250 ms until all complete or @timeoutSecs@
-- expires.
waitInflight :: TVar (Map Int (Async ())) -> Int -> IO ()
waitInflight inflightV timeoutSecs = go (timeoutSecs * 4)
  where
    go 0     = pure ()
    go ticks = do
      reapCompleted inflightV
      remaining <- readTVarIO inflightV
      unless (Map.null remaining) $ do
        threadDelay 250_000
        go (ticks - 1)

-- ---------------------------------------------------------------------------
-- Main loop
-- ---------------------------------------------------------------------------

schedulerLoop
  :: AppEnv
  -> TVar Bool
  -> TVar (Map Int (Async ()))
  -> TVar Int
  -> IO ()
schedulerLoop env shutdown inflightV nextIdV = do
  sleepUntilNextMinute
  go
  where
    go = do
      shouldStop <- readTVarIO shutdown
      unless shouldStop $ do
        now <- getCurrentTime
        fireMatchingPrograms env now inflightV nextIdV
        reapCompleted inflightV
        sleepUntilNextMinute
        go

-- ---------------------------------------------------------------------------
-- Firing logic
-- ---------------------------------------------------------------------------

-- | Query all programs and fire a run for each Active program with a
-- TriggerCron node whose schedule matches @now@. Each run is isolated in its
-- own async so that failures do not affect other programs.
fireMatchingPrograms
  :: AppEnv
  -> UTCTime
  -> TVar (Map Int (Async ()))
  -> TVar Int
  -> IO ()
fireMatchingPrograms env now inflightV nextIdV = do
  programs <- runAppM env listPrograms
  let candidates = concatMap (matchingTriggers now) programs
  forM_ candidates $ \program -> do
    rid <- atomically $ do
      n <- readTVar nextIdV
      writeTVar nextIdV (n + 1)
      pure n
    a <- async $ fireRun env program
    atomically $ modifyTVar' inflightV (Map.insert rid a)

-- | Return the program in a singleton list if it is Active and has at least
-- one TriggerCron node whose schedule matches @now@. Returns @[]@ otherwise.
-- Exported for unit testing.
matchingTriggers :: UTCTime -> Program -> [Program]
matchingTriggers now program
  | programState program /= Active = []
  | otherwise =
      if any (nodeCronMatches now) (Map.elems (graphNodes (programGraph program)))
        then [program]
        else []

-- | True if this node is a TriggerCron with a cron expression that matches
-- @now@. Returns False for non-trigger nodes, non-cron triggers, missing
-- schedules, and unparseable expressions.
nodeCronMatches :: UTCTime -> Node -> Bool
nodeCronMatches now node =
  case nodeType node of
    TriggerNode cfg
      | triggerType cfg == TriggerCron ->
          case triggerSchedule cfg of
            Nothing   -> False
            Just expr ->
              case parseCronSchedule expr of
                Left  _  -> False
                Right cs -> scheduleMatches cs now
    _ -> False

-- | Start a run for @program@ and wait for it to finish. Exceptions are
-- caught and logged so the scheduler loop and other programs are unaffected.
fireRun :: AppEnv -> Program -> IO ()
fireRun env program = do
  result <- try runIt :: IO (Either SomeException ())
  case result of
    Right () -> pure ()
    Left err ->
      runAppM env $
        logMsg LogWarn $
          "Scheduler: run failed for program "
            <> programLabel program
            <> ": "
            <> T.pack (show err)
  where
    runIt :: IO ()
    runIt = do
      ctx <- runAppM env (startRun program Nothing)
      waitForRun ctx

-- ---------------------------------------------------------------------------
-- Inflight cleanup
-- ---------------------------------------------------------------------------

-- | Remove completed asyncs from the inflight map to prevent unbounded growth.
reapCompleted :: TVar (Map Int (Async ())) -> IO ()
reapCompleted inflightV = do
  inflight <- readTVarIO inflightV
  stillRunning <- filterMapM (\a -> isNothing <$> poll a) inflight
  atomically $ writeTVar inflightV stillRunning

-- | Filter a Map by running an IO predicate on each value.
filterMapM :: (Monad m, Ord k) => (v -> m Bool) -> Map k v -> m (Map k v)
filterMapM p m = do
  checks <- mapM p m
  pure $ Map.intersectionWith const m (Map.filter id checks)

-- ---------------------------------------------------------------------------
-- Sleep helpers
-- ---------------------------------------------------------------------------

-- | Sleep until the next full-minute boundary (:00 seconds).
-- Waits at least 1 s past the boundary to avoid double-firing.
sleepUntilNextMinute :: IO ()
sleepUntilNextMinute = do
  now <- getCurrentTime
  let secsIntoMinute = toDouble (utctDayTime now) `fmod` 60.0
      secsToNext     = 60.0 - secsIntoMinute
      -- Within 1 s of the boundary: skip forward to avoid double-firing.
      adjusted       = if secsToNext <= 1.0 then secsToNext + 60.0 else secsToNext
      microseconds   = ceiling (adjusted * 1_000_000) :: Int
  threadDelay microseconds
  where
    -- 'utctDayTime' returns 'DiffTime'; 'realToFrac' converts it to seconds.
    toDouble t = realToFrac t :: Double
    fmod a b   = a - fromIntegral (floor (a / b) :: Int) * b

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

programLabel :: Program -> Text
programLabel p =
  let (ProgramId t) = programId p
  in "\"" <> programName p <> "\" (" <> t <> ")"

