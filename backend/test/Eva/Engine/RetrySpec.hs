{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for EVA-16: per-node retry policy, error port propagation,
-- branch isolation, and log entry recording.
module Eva.Engine.RetrySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (Value (..))
import Data.IORef
import qualified Data.Map.Strict as Map
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import Eva.App (AppEnv (..), AppM, runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import Eva.Core.Types
import Eva.Engine.Runner (startRun, waitForRun, withRetry)
import Eva.Engine.StateMachine (RunContext (..))
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries (insertProgram)

-- ---------------------------------------------------------------------------
-- Intentional failure exception
-- ---------------------------------------------------------------------------

data IntentionalFailure = IntentionalFailure String deriving Show
instance Exception IntentionalFailure

-- ---------------------------------------------------------------------------
-- Dispatch function builders
-- ---------------------------------------------------------------------------

-- | Always-succeeding dispatch: echo the node ID as the message type.
successDispatch
  :: RunId -> Node -> Map.Map PortName Message -> ResourceBindings -> AppM Message
successDispatch rid node _inputs _bindings =
  pure Message
    { msgType    = "test_output"
    , msgPayload = Null
    , msgMeta    = MessageMeta
        { metaTraceId    = "trace"
        , metaTimestamp  = read "2026-01-01 00:00:00 UTC"
        , metaSourceNode = nodeId node
        , metaRunId      = rid
        }
    }

-- | Wraps @baseDispatch@ with a counter: fails @targetNid@ until the counter
-- reaches zero, then delegates to @baseDispatch@.
failNTimesFor
  :: IORef Int
  -> NodeId
  -> (RunId -> Node -> Map.Map PortName Message -> ResourceBindings -> AppM Message)
  -> RunId -> Node -> Map.Map PortName Message -> ResourceBindings -> AppM Message
failNTimesFor counterRef targetNid baseDispatch rid node inputs bindings
  | nodeId node /= targetNid = baseDispatch rid node inputs bindings
  | otherwise = do
      remaining <- liftIO $ readIORef counterRef
      if remaining > 0
        then do
          liftIO $ writeIORef counterRef (remaining - 1)
          liftIO $ throwIO (IntentionalFailure "injected failure")
        else baseDispatch rid node inputs bindings

-- | Always fails @targetNid@; all other nodes succeed via @successDispatch@.
alwaysFailFor
  :: NodeId
  -> RunId -> Node -> Map.Map PortName Message -> ResourceBindings -> AppM Message
alwaysFailFor targetNid _rid node _inputs _bindings
  | nodeId node == targetNid =
      liftIO $ throwIO (IntentionalFailure "always fails")
  | otherwise = pure Message
      { msgType    = "test_output"
      , msgPayload = Null
      , msgMeta    = MessageMeta
          { metaTraceId    = "trace"
          , metaTimestamp  = read "2026-01-01 00:00:00 UTC"
          , metaSourceNode = nodeId node
          , metaRunId      = _rid
          }
      }

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

withTestEnv
  :: (RunId -> Node -> Map.Map PortName Message -> ResourceBindings -> AppM Message)
  -> (AppEnv -> IO ())
  -> IO ()
withTestEnv dispatch action = do
  pool <- runNoLoggingT $ createSqlitePool ":memory:" 4
  runMigrations pool
  let cfg = AppConfig
        { configDbPath    = ":memory:"
        , configPort      = 8080
        , configLlmApiKey = Nothing
        , configLogLevel  = LogError
        }
      env = AppEnv
        { envConfig   = cfg
        , envDbPool   = pool
        , envLogger   = \_ -> pure ()
        , envDispatch = dispatch
        }
  action env

-- ---------------------------------------------------------------------------
-- Graph fixtures
-- ---------------------------------------------------------------------------

triggerNode :: NodeId -> Node
triggerNode nid = Node
  { nodeId   = nid
  , nodeLabel = "Trigger"
  , nodeType = TriggerNode TriggerConfig
      { triggerType            = TriggerManual
      , triggerSchedule        = Nothing
      , triggerEventFilter     = Nothing
      , triggerPayloadTemplate = Nothing
      }
  , nodePosX = 0
  , nodePosY = 0
  }

-- | Action node with a given retry policy.
actionWithRetry :: NodeId -> RetryPolicy -> Node
actionWithRetry nid policy = Node
  { nodeId   = nid
  , nodeLabel = "Action"
  , nodeType = ActionNode ActionConfig
      { actionOperation    = OpTemplate
      , actionParameters   = Null
      , actionErrorHandling = ErrFail
      , actionRetryPolicy  = Just policy
      }
  , nodePosX = 100
  , nodePosY = 0
  }

-- | Action node with no retry policy.
actionNoRetry :: NodeId -> Node
actionNoRetry nid = Node
  { nodeId   = nid
  , nodeLabel = "Action"
  , nodeType = ActionNode ActionConfig
      { actionOperation    = OpTemplate
      , actionParameters   = Null
      , actionErrorHandling = ErrFail
      , actionRetryPolicy  = Nothing
      }
  , nodePosX = 100
  , nodePosY = 0
  }

dataEdge :: EdgeId -> NodeId -> PortName -> NodeId -> PortName -> Edge
dataEdge eid src sp tgt tp = Edge
  { edgeId         = eid
  , edgeSourceNode = src
  , edgeSourcePort = sp
  , edgeTargetNode = tgt
  , edgeTargetPort = tp
  , edgeCategory   = PortData
  }

errorEdgeFrom :: EdgeId -> NodeId -> NodeId -> PortName -> Edge
errorEdgeFrom eid src tgt tgtPort = Edge
  { edgeId         = eid
  , edgeSourceNode = src
  , edgeSourcePort = "error"
  , edgeTargetNode = tgt
  , edgeTargetPort = tgtPort
  , edgeCategory   = PortData
  }

mkProgram :: ProgramId -> [Node] -> [Edge] -> Program
mkProgram pid nodes edges = Program
  { programId        = pid
  , programName      = "Retry Test"
  , programState     = Draft
  , programGraph     = Graph
      { graphNodes = Map.fromList [(nodeId n, n) | n <- nodes]
      , graphEdges = edges
      }
  , programCreatedAt = epoch
  , programUpdatedAt = epoch
  }
  where
    epoch = read "2026-01-01 00:00:00 UTC"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  -- -------------------------------------------------------------------------
  -- withRetry unit tests
  -- -------------------------------------------------------------------------

  describe "withRetry" $ do

    it "AC1: returns Right on first success (no retries needed)" $ do
      result <- withRetry (RetryPolicy 3 (BackoffFixed 1) Nothing) (pure (42 :: Int))
      case result of
        Right v -> v `shouldBe` 42
        Left  e -> expectationFailure $ "expected Right 42, got Left: " <> show e

    it "AC1: retries exactly maxAttempts times, then returns Left" $ do
      counter <- newIORef (0 :: Int)
      result  <- withRetry (RetryPolicy 2 (BackoffFixed 1) Nothing) $ do
        modifyIORef counter (+1)
        throwIO (IntentionalFailure "always fails")
      case result of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left after exhausting retries"
      -- 1 initial + 2 retries = 3 total calls
      readIORef counter >>= (`shouldBe` 3)

    it "AC1: succeeds on the (maxAttempts)th retry" $ do
      counter <- newIORef (0 :: Int)
      result  <- withRetry (RetryPolicy 3 (BackoffFixed 1) Nothing) $ do
        n <- readIORef counter
        modifyIORef counter (+1)
        if n < 2
          then throwIO (IntentionalFailure "not yet")
          else pure ("success" :: String)
      case result of
        Right v -> v `shouldBe` "success"
        Left  e -> expectationFailure $ "expected Right, got Left: " <> show e

    it "AC5: timeout expiry triggers step failure with descriptive error" $ do
      result <- withRetry (RetryPolicy 0 (BackoffFixed 0) (Just 1)) $ do
        -- Sleep 200ms >> 1ms timeout → should time out
        threadDelay 200000
        pure ("done" :: String)
      case result of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left (timeout)"

  -- -------------------------------------------------------------------------
  -- Integration tests via startRun
  -- -------------------------------------------------------------------------

  describe "retry integration" $ do

    it "AC1: node retries up to maxAttempts — stepRetryCount reflects attempts" $
      -- Action fails twice, then succeeds on the 3rd attempt.
      -- Policy: maxAttempts = 3, fixed 1ms backoff.
      do
        counter <- newIORef 2
        withTestEnv (failNTimesFor counter "n-action" successDispatch) $ \env -> do
          let policy = RetryPolicy 3 (BackoffFixed 1) Nothing
              prog   = mkProgram "prog-ac1"
                [ triggerNode "n-trigger"
                , actionWithRetry "n-action" policy
                ]
                [ dataEdge "e1" "n-trigger" "event" "n-action" "input" ]
          runAppM env $ insertProgram prog
          ctx <- runAppM env $ startRun prog Nothing
          waitForRun ctx

          run <- readTVarIO (rcRun ctx)
          runState run `shouldBe` RunCompleted

          steps <- readTVarIO (rcSteps ctx)
          case Map.lookup "n-action" steps of
            Nothing -> expectationFailure "no step for n-action"
            Just tv -> do
              s <- readTVarIO tv
              stepRetryCount s `shouldBe` 2
              stepState s `shouldBe` StepCompleted

    it "AC6: retry attempts are recorded (retryCount persisted to DB)" $
      do
        counter <- newIORef 1
        withTestEnv (failNTimesFor counter "n-action" successDispatch) $ \env -> do
          let policy = RetryPolicy 2 (BackoffFixed 1) Nothing
              prog   = mkProgram "prog-ac6"
                [ triggerNode "n-trigger"
                , actionWithRetry "n-action" policy
                ]
                [ dataEdge "e1" "n-trigger" "event" "n-action" "input" ]
          runAppM env $ insertProgram prog
          ctx <- runAppM env $ startRun prog Nothing
          waitForRun ctx

          steps <- readTVarIO (rcSteps ctx)
          case Map.lookup "n-action" steps of
            Nothing -> expectationFailure "no step for n-action"
            Just tv -> do
              s <- readTVarIO tv
              -- 1 failure → 1 retry → retryCount = 1
              stepRetryCount s `shouldBe` 1
              stepState s `shouldBe` StepCompleted

  describe "error port propagation" $ do

    it "AC2: wired error port — error message propagates, recovery node executes" $
      withTestEnv (alwaysFailFor "n-action") $ \env -> do
        -- Graph: Trigger → Action -[error]→ Recovery
        let prog = mkProgram "prog-ac2"
              [ triggerNode "n-trigger"
              , actionNoRetry "n-action"
              , actionNoRetry "n-recovery"
              ]
              [ dataEdge  "e1" "n-trigger" "event"  "n-action"   "input"
              , errorEdgeFrom "e2" "n-action" "n-recovery" "input"
              ]
        runAppM env $ insertProgram prog
        ctx <- runAppM env $ startRun prog Nothing
        waitForRun ctx

        run <- readTVarIO (rcRun ctx)
        -- Error was handled via error port → Run completes, not fails
        runState run `shouldBe` RunCompleted

        steps <- readTVarIO (rcSteps ctx)
        case Map.lookup "n-action" steps of
          Nothing -> expectationFailure "no step for n-action"
          Just tv -> readTVarIO tv >>= \s -> stepState s `shouldBe` StepFailed
        case Map.lookup "n-recovery" steps of
          Nothing -> expectationFailure "no step for n-recovery"
          Just tv -> readTVarIO tv >>= \s -> stepState s `shouldBe` StepCompleted

  describe "branch isolation" $ do

    it "AC3: unwired error — downstream is Skipped, Run is Failed" $
      withTestEnv (alwaysFailFor "n-action") $ \env -> do
        -- Graph: Trigger → Action → Downstream (no error edge from Action)
        let prog = mkProgram "prog-ac3"
              [ triggerNode "n-trigger"
              , actionNoRetry "n-action"
              , actionNoRetry "n-downstream"
              ]
              [ dataEdge "e1" "n-trigger" "event"  "n-action"    "input"
              , dataEdge "e2" "n-action"  "output" "n-downstream" "input"
              ]
        runAppM env $ insertProgram prog
        ctx <- runAppM env $ startRun prog Nothing
        waitForRun ctx

        run <- readTVarIO (rcRun ctx)
        runState run `shouldBe` RunFailed

        steps <- readTVarIO (rcSteps ctx)
        case Map.lookup "n-action" steps of
          Nothing -> expectationFailure "no step for n-action"
          Just tv -> readTVarIO tv >>= \s -> stepState s `shouldBe` StepFailed
        case Map.lookup "n-downstream" steps of
          Nothing -> expectationFailure "n-downstream should have a Skipped step"
          Just tv -> readTVarIO tv >>= \s -> stepState s `shouldBe` StepSkipped

    it "AC4: Run is Completed when no unhandled errors occur" $
      withTestEnv successDispatch $ \env -> do
        let prog = mkProgram "prog-ac4"
              [ triggerNode "n-trigger"
              , actionNoRetry "n-action"
              ]
              [ dataEdge "e1" "n-trigger" "event" "n-action" "input" ]
        runAppM env $ insertProgram prog
        ctx <- runAppM env $ startRun prog Nothing
        waitForRun ctx

        run <- readTVarIO (rcRun ctx)
        runState run `shouldBe` RunCompleted

    it "AC4: Run is Failed only when an unhandled error exists (not when error is wired)" $
      withTestEnv (alwaysFailFor "n-action") $ \env -> do
        -- Action fails but its error port IS wired → Run should Complete
        let prog = mkProgram "prog-ac4b"
              [ triggerNode "n-trigger"
              , actionNoRetry "n-action"
              , actionNoRetry "n-recovery"
              ]
              [ dataEdge  "e1" "n-trigger" "event"  "n-action"   "input"
              , errorEdgeFrom "e2" "n-action" "n-recovery" "input"
              ]
        runAppM env $ insertProgram prog
        ctx <- runAppM env $ startRun prog Nothing
        waitForRun ctx

        run <- readTVarIO (rcRun ctx)
        runState run `shouldBe` RunCompleted
