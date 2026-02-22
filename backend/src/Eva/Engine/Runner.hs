{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Execution engine: RunContext creation, graph walker, message propagation,
-- terminal detection, retry policy, and error port propagation.
--
-- Execution model:
--   1. 'startRun' creates a RunContext with per-port TMVar mailboxes, transitions
--      the Run to Running, executes Trigger nodes synchronously, then forks the
--      graph walker as a background async.
--   2. The graph walker loops via STM: atomically finds nodes whose required data
--      inputs are all filled, consumes those messages, forks an async per ready
--      node, waits for the batch, then repeats.
--   3. 'executeNodeStep' creates/transitions a Step, calls the injected dispatch
--      function with the node's retry policy and timeout, places the output into
--      downstream mailboxes, and signals done when all terminal nodes finish.
--   4. Resource edges are resolved as static context at dispatch time — they do
--      not get mailboxes and do not gate execution readiness.
--   5. Error handling: on final dispatch failure, if a wired "error" output edge
--      exists the error message is propagated downstream (branch continues).
--      Otherwise 'skipDescendants' marks downstream nodes as Skipped and
--      'rcHasUnhandledError' is set, causing the Run to transition to Failed.
module Eva.Engine.Runner
  ( -- * Run lifecycle
    startRun
  , waitForRun

    -- * Exposed for testing
  , resolveResourceBindings
  , withRetry
  , NodeStepFailure (..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitCatch)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, fromException, throwIO, try)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Data.Aeson (Value, object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import System.Timeout (timeout)

import Eva.Api.WebSocket
  ( logEntryEvent
  , runStateEvent
  , stepStateEvent
  )
import Eva.App (AppEnv, AppM, broadcastEvent, logMsg, registerRun, runAppM, unregisterRun)
import qualified Eva.App as App
import Eva.Config (LogLevel (..))
import Eva.Core.Graph
  ( dataEdgesOf
  , hasErrorEdge
  , nodeDataOutputPort
  , reachableFrom
  , requiredDataInputs
  , rootNodes
  , terminalNodes
  )
import Eva.Core.Types
import Eva.Engine.Handlers.Connector (resolveConnectorRunner)
import Eva.Engine.StateMachine
  ( RunContext (..)
  , markUnhandledError
  , newRunContext
  , transitionRun
  , transitionStep
  , waitForRun
  )
import Eva.Persistence.Queries
  ( insertLogEntry
  , insertRun
  , insertStep
  , updateStepRetryCount
  )

-- ---------------------------------------------------------------------------
-- NodeStepFailure exception
-- ---------------------------------------------------------------------------

-- | Thrown by 'executeNodeStep' when a node fails after all retries and its
-- error port is not wired. Carries the node ID so the walker can identify
-- which branch to skip.
data NodeStepFailure = NodeStepFailure
  { nsfNodeId :: NodeId
  , nsfCause  :: SomeException
  }
  deriving Show

instance Exception NodeStepFailure

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Create and start a new Run for the given program.
-- Forks the graph walker as a background async and returns the RunContext
-- immediately (the Run is already in Running state when this returns).
-- Use 'waitForRun' to block until the graph walker completes.
startRun :: Program -> Maybe Value -> AppM RunContext
startRun program triggerPayload = do
  env <- ask

  rid <- liftIO $ RunId . UUID.toText <$> nextRandom
  now <- liftIO getCurrentTime
  let graph = programGraph program
      run = Run
        { runId          = rid
        , runProgramId   = programId program
        , runState       = RunPending
        , runTriggerInfo = triggerPayload
        , runStartedAt   = Nothing
        , runFinishedAt  = Nothing
        }

  let dataPorts =
        [ (nid, port)
        | (nid, node) <- Map.toList (graphNodes graph)
        , port <- requiredDataInputs (nodeType node)
        ]

  ctx <- liftIO $ newRunContext run dataPorts

  -- Register the broadcast channel before the run starts so WS clients
  -- can subscribe as soon as the run is created.
  registerRun rid (rcBroadcast ctx)

  insertRun run
  _ <- transitionRun (rcRun ctx) RunRunning now
  broadcastEvent rid (runStateEvent rid RunRunning now)

  let dEdges = dataEdgesOf graph
  forM_ (rootNodes graph) $ \nid ->
    case Map.lookup nid (graphNodes graph) of
      Nothing   -> pure ()
      Just node -> case nodeType node of
        TriggerNode _ -> do
          mOutMsg <- executeNodeStep env ctx graph node Map.empty
          forM_ mOutMsg $ \outMsg ->
            liftIO $ placeMessages ctx dEdges nid (nodeDataOutputPort (nodeType node)) outMsg
        _ -> pure ()

  _ <- liftIO $ async $ runAppM env $ graphWalkerLoop ctx graph
  pure ctx

-- ---------------------------------------------------------------------------
-- Graph walker
-- ---------------------------------------------------------------------------

graphWalkerLoop :: RunContext -> Graph -> AppM ()
graphWalkerLoop ctx graph = do
  env <- ask
  let dEdges = dataEdgesOf graph
      willHaveStep (_nid, node) = case nodeType node of
        TriggerNode _ -> True
        _             -> not (null (requiredDataInputs (nodeType node)))
      executableNodes =
        Set.fromList
          [ nid | (nid, node) <- Map.toList (graphNodes graph)
                , willHaveStep (nid, node) ]
      terminals = Set.fromList (terminalNodes graph) `Set.intersection` executableNodes
  go env dEdges terminals executableNodes
  where
    go env dEdges terminals executableNodes = do
      readyInputs <- liftIO $ atomically $ do
        done <- readTVar (rcAllDone ctx)
        if done
          then pure []
          else do
            dispatched <- readTVar (rcDispatched ctx)
            pairs      <- collectReadyInputs ctx graph dispatched
            when (null pairs) retry
            modifyTVar (rcDispatched ctx) (Set.union (Set.fromList (map fst pairs)))
            pure pairs

      if null readyInputs
        then finishRun
        else do
          asyncsWithIds <- forM readyInputs $ \(nid, inputs) ->
            (nid,) <$> liftIO (async $ runAppM env $
              case Map.lookup nid (graphNodes graph) of
                Nothing   -> pure ()
                Just node -> do
                  mOutMsg <- executeNodeStep env ctx graph node inputs
                  forM_ mOutMsg $ \outMsg ->
                    liftIO $ placeMessages ctx dEdges nid
                               (nodeDataOutputPort (nodeType node)) outMsg
                  liftIO $ checkAndSignalDone ctx terminals)

          results <- liftIO $ mapM (\(nid, a) -> (nid,) <$> waitCatch a) asyncsWithIds

          forM_ results $ \(walkerNid, outcome) ->
            case outcome of
              Right () -> pure ()
              Left e   -> do
                -- Use node ID from NodeStepFailure if available, else walkerNid.
                let nid = case fromException e of
                            Just (NodeStepFailure failNid _) -> failNid
                            Nothing                          -> walkerNid
                skipDescendants ctx graph executableNodes nid
                liftIO $ markUnhandledError ctx

          go env dEdges terminals executableNodes

    finishRun = do
      run <- liftIO $ readTVarIO (rcRun ctx)
      case runState run of
        RunRunning -> do
          hasErr <- liftIO $ readTVarIO (rcHasUnhandledError ctx)
          now    <- liftIO getCurrentTime
          let finalState = if hasErr then RunFailed else RunCompleted
          _      <- transitionRun (rcRun ctx) finalState now
          broadcastEvent (rcRunId ctx) (runStateEvent (rcRunId ctx) finalState now)
          unregisterRun (rcRunId ctx)
        _ -> pure ()

-- ---------------------------------------------------------------------------
-- Node step execution
-- ---------------------------------------------------------------------------

-- | Execute a single node within a run.
-- Returns 'Just outMsg' on success, or 'Nothing' if the error was propagated
-- via the node's wired "error" output port.
-- Throws 'NodeStepFailure' if the node fails with no wired error port.
executeNodeStep
  :: AppEnv
  -> RunContext
  -> Graph
  -> Node
  -> Map PortName Message
  -> AppM (Maybe Message)
executeNodeStep env ctx graph node inputs = do
  sid <- liftIO $ StepId . UUID.toText <$> nextRandom
  now <- liftIO getCurrentTime
  let step = Step
        { stepId         = sid
        , stepRunId      = rcRunId ctx
        , stepNodeId     = nodeId node
        , stepState      = StepPending
        , stepInput      = Just (toJSON inputs)
        , stepOutput     = Nothing
        , stepError      = Nothing
        , stepRetryCount = 0
        , stepStartedAt  = Nothing
        , stepFinishedAt = Nothing
        }
  stepTVar <- liftIO $ do
    tv <- newTVarIO step
    atomically $ modifyTVar (rcSteps ctx) (Map.insert (nodeId node) tv)
    pure tv
  insertStep step

  stepRunning <- transitionStep stepTVar StepRunning now
  broadcastEvent (rcRunId ctx)
    (stepStateEvent (rcRunId ctx) (nodeId node) (stepId stepRunning) StepRunning now)

  bindings0 <- resolveResourceBindings graph (nodeId node)
  cache     <- liftIO $ readTVarIO (rcKnowledgeCache ctx)
  let bindings = bindings0 { rbKnowledgeDynamic = dynamicKnowledge graph cache (nodeId node) }
  let mPolicy  = nodeRetryPolicy (nodeType node)

  dispatch <- asks App.envDispatch
  result   <- liftIO $
    dispatchWithRetry env dispatch mPolicy sid ctx node inputs bindings

  case result of
    Right outMsg -> do
      now2 <- liftIO getCurrentTime
      liftIO $ atomically $ do
        modifyTVar stepTVar (\s -> s { stepOutput = Just (toJSON outMsg) })
        -- Cache resolved content for Knowledge nodes so that downstream Agents
        -- can include it via rbKnowledgeDynamic (populated in executeNodeStep).
        case nodeType node of
          KnowledgeNode _ ->
            modifyTVar (rcKnowledgeCache ctx)
              (Map.insert (nodeId node) (msgPayload outMsg))
          _ -> pure ()
      stepDone <- transitionStep stepTVar StepCompleted now2
      broadcastEvent (rcRunId ctx)
        (stepStateEvent (rcRunId ctx) (nodeId node) (stepId stepDone) StepCompleted now2)
      pure (Just outMsg)

    Left finalErr -> do
      now2 <- liftIO getCurrentTime
      let errText = T.pack (show finalErr)
      liftIO $ atomically $
        modifyTVar stepTVar (\s -> s { stepError = Just errText })
      stepFailed <- transitionStep stepTVar StepFailed now2
      broadcastEvent (rcRunId ctx)
        (stepStateEvent (rcRunId ctx) (nodeId node) (stepId stepFailed) StepFailed now2)

      if hasErrorEdge graph (nodeId node)
        then do
          traceId <- liftIO $ UUID.toText <$> nextRandom
          errNow  <- liftIO getCurrentTime
          let meta   = MessageMeta
                { metaTraceId    = traceId
                , metaTimestamp  = errNow
                , metaSourceNode = nodeId node
                , metaRunId      = rcRunId ctx
                }
              errMsg = Message "error" (Aeson.object ["error" .= errText]) meta
              dEdges = dataEdgesOf graph
          liftIO $ placeMessages ctx dEdges (nodeId node) "error" errMsg
          pure Nothing
        else
          liftIO $ throwIO (NodeStepFailure (nodeId node) finalErr)

-- ---------------------------------------------------------------------------
-- Dispatch with retry
-- ---------------------------------------------------------------------------

-- | Dispatch a node's handler, applying the optional retry policy.
-- Logs each failed attempt to log_entries and updates stepRetryCount in the DB.
-- Returns 'Right msg' on success or 'Left err' after all attempts exhausted.
dispatchWithRetry
  :: AppEnv
  -> (RunId -> Node -> Map PortName Message -> ResourceBindings -> AppM Message)
  -> Maybe RetryPolicy
  -> StepId
  -> RunContext
  -> Node
  -> Map PortName Message
  -> ResourceBindings
  -> IO (Either SomeException Message)
dispatchWithRetry env dispatch mPolicy sid ctx node inputs bindings =
  go 0
  where
    (maxAttempts, mTimeoutMs, backoff) = case mPolicy of
      Nothing     -> (0, Nothing, BackoffFixed 0)
      Just policy -> (retryMaxAttempts policy, retryTimeoutMs policy, retryBackoff policy)

    runDispatch :: IO (Either SomeException Message)
    runDispatch = try $ case mTimeoutMs of
      Nothing -> runAppM env $ dispatch (rcRunId ctx) node inputs bindings
      Just ms -> do
        mResult <- timeout (ms * 1000) $
          runAppM env $ dispatch (rcRunId ctx) node inputs bindings
        case mResult of
          Just r  -> pure r
          Nothing -> ioError (userError ("timeout after " <> show ms <> "ms"))

    go :: Int -> IO (Either SomeException Message)
    go n = do
      result <- runDispatch
      case result of
        Right v -> pure (Right v)
        Left  e
          | n < maxAttempts -> do
              let attemptNum = n + 1
                  nodeIdTxt  = let (NodeId t) = nodeId node in t
                  msg = T.concat
                    [ "Retry ", T.pack (show attemptNum)
                    , "/",      T.pack (show maxAttempts)
                    , " for node ", nodeIdTxt
                    , ": ", T.pack (show e)
                    ]
                  dat = object
                    [ "attempt"     .= attemptNum
                    , "maxAttempts" .= maxAttempts
                    , "error"       .= T.pack (show e)
                    , "nodeId"      .= nodeIdTxt
                    ]
              runAppM env $ insertLogEntry sid "warn" msg (Just dat)
              runAppM env $ updateStepRetryCount sid attemptNum
              retryNow <- getCurrentTime
              runAppM env $
                broadcastEvent (rcRunId ctx)
                  (logEntryEvent (rcRunId ctx) sid "warn" msg retryNow)
              -- Update in-memory step TVar.
              steps <- readTVarIO (rcSteps ctx)
              case Map.lookup (nodeId node) steps of
                Just tv -> atomically $ modifyTVar tv (\s -> s { stepRetryCount = attemptNum })
                Nothing -> pure ()
              threadDelay (backoffDelay backoff n)
              go (n + 1)
          | otherwise -> pure (Left e)

-- | Exposed for testing: apply a retry policy to an arbitrary IO action.
-- Returns 'Right' on the first success, 'Left' after all attempts exhausted.
withRetry :: forall a. RetryPolicy -> IO a -> IO (Either SomeException a)
withRetry policy action = go 0
  where
    maxAttempts = retryMaxAttempts policy
    mTimeoutMs  = retryTimeoutMs policy
    boff        = retryBackoff policy

    go :: Int -> IO (Either SomeException a)
    go n = do
      result <- try $ case mTimeoutMs of
        Nothing -> action
        Just ms -> do
          mResult <- timeout (ms * 1000) action
          case mResult of
            Just r  -> pure r
            Nothing -> ioError (userError ("timeout after " <> show ms <> "ms"))
      case result of
        Right v -> pure (Right v)
        Left  e
          | n < maxAttempts -> do
              threadDelay (backoffDelay boff n)
              go (n + 1)
          | otherwise -> pure (Left e)

-- | Compute the backoff delay in microseconds for attempt @n@ (0-indexed).
-- BackoffFixed ms: constant delay.
-- BackoffExponential baseMs capMs: @baseMs * 2^n@ ms, capped at @capMs@ ms.
backoffDelay :: BackoffStrategy -> Int -> Int
backoffDelay (BackoffFixed ms)               _n = ms * 1000
backoffDelay (BackoffExponential baseMs capMs) n =
  min capMs (baseMs * (2 ^ n)) * 1000

-- | Extract the retry policy from a node's type-specific config.
nodeRetryPolicy :: NodeType -> Maybe RetryPolicy
nodeRetryPolicy (AgentNode   cfg) = agentRetryPolicy  cfg
nodeRetryPolicy (ActionNode  cfg) = actionRetryPolicy cfg
nodeRetryPolicy _                 = Nothing

-- ---------------------------------------------------------------------------
-- Mailbox helpers
-- ---------------------------------------------------------------------------

-- | Place a message into the downstream mailboxes of all data edges that
-- originate from @srcNode@ on @srcPort@. Silently skips edges whose target
-- port has no mailbox (resource ports, unmapped ports).
placeMessages :: RunContext -> [Edge] -> NodeId -> PortName -> Message -> IO ()
placeMessages ctx dEdges srcNode srcPort msg =
  atomically $
    forM_ dEdges $ \e ->
      when (edgeSourceNode e == srcNode && edgeSourcePort e == srcPort) $
        case Map.lookup (edgeTargetNode e, edgeTargetPort e) (rcMailboxes ctx) of
          Just tmv -> putTMVar tmv msg
          Nothing  -> pure ()

-- | STM action: scan all un-dispatched nodes and return those whose required
-- data-input mailboxes are all full. Atomically consumes the messages.
collectReadyInputs
  :: RunContext
  -> Graph
  -> Set NodeId
  -> STM [(NodeId, Map PortName Message)]
collectReadyInputs ctx graph dispatched =
  fmap (mapMaybe id) $
    forM (Map.toList (graphNodes graph)) $ \(nid, node) ->
      if Set.member nid dispatched
        then pure Nothing
        else do
          let required = requiredDataInputs (nodeType node)
          case required of
            [] -> pure Nothing
            _  -> do
              peeks <- forM required $ \port ->
                case Map.lookup (nid, port) (rcMailboxes ctx) of
                  Nothing  -> pure Nothing
                  Just tmv -> tryReadTMVar tmv
              if all (/= Nothing) peeks
                then do
                  msgs <- forM required $ \port ->
                    case Map.lookup (nid, port) (rcMailboxes ctx) of
                      Nothing  -> pure (port, emptyMessage)
                      Just tmv -> (port,) <$> takeTMVar tmv
                  pure $ Just (nid, Map.fromList msgs)
                else pure Nothing
  where
    emptyMessage = Message "" (error "unreachable: mailbox key missing") dummyMeta
    dummyMeta    = error "unreachable: mailbox key missing"

-- ---------------------------------------------------------------------------
-- Resource binding resolution
-- ---------------------------------------------------------------------------

-- | Resolve resource bindings for @nid@: collect Knowledge/Connector configs
-- from resource edges targeting this node, then resolve ConnectorRunners
-- (credential lookup + runner construction) for each ConnectorConfig.
-- Connector resolution errors are logged as warnings and the failed runner
-- is omitted from rbConnectorRunners (the config is still in rbConnectors).
-- Note: 'rbKnowledgeDynamic' is left empty here; it is populated in
-- 'executeNodeStep' by reading 'rcKnowledgeCache' after this call.
resolveResourceBindings :: Graph -> NodeId -> AppM ResourceBindings
resolveResourceBindings graph nid = do
  let resourceEdges =
        filter
          (\e -> edgeCategory e == PortResource && edgeTargetNode e == nid)
          (graphEdges graph)
      sourceNodes =
        mapMaybe
          (\e -> Map.lookup (edgeSourceNode e) (graphNodes graph))
          resourceEdges
      knowledge  = [cfg | KnowledgeNode cfg <- map nodeType sourceNodes]
      connectors = [cfg | ConnectorNode cfg  <- map nodeType sourceNodes]
  runners <- resolveConnectors connectors
  pure ResourceBindings
         { rbKnowledge        = knowledge
         , rbKnowledgeDynamic = []
         , rbConnectors       = connectors
         , rbConnectorRunners = runners
         }

-- | Build the dynamic Knowledge content list for @targetNid@ by looking up
-- completed UpstreamPort Knowledge steps in the per-run cache.
-- Only Knowledge nodes wired via resource edges to @targetNid@ with an
-- UpstreamPort source are considered.
dynamicKnowledge :: Graph -> Map NodeId Value -> NodeId -> [Value]
dynamicKnowledge graph cache targetNid =
  let resourceEdges =
        filter
          (\e -> edgeCategory e == PortResource && edgeTargetNode e == targetNid)
          (graphEdges graph)
      sourceNodes =
        mapMaybe
          (\e -> (edgeSourceNode e,) <$> Map.lookup (edgeSourceNode e) (graphNodes graph))
          resourceEdges
  in [ v
     | (srcNid, srcNode) <- sourceNodes
     , KnowledgeNode cfg <- [nodeType srcNode]
     , knowledgeSource cfg == UpstreamPort
     , Just v <- [Map.lookup srcNid cache]
     ]

-- | Attempt to resolve each ConnectorConfig into a live ConnectorRunner.
-- Failed resolutions are logged at warn level and excluded from the result.
resolveConnectors :: [ConnectorConfig] -> AppM [ConnectorRunner]
resolveConnectors cfgs =
  fmap (mapMaybe id) $
    forM cfgs $ \cfg -> do
      result <- resolveConnectorRunner cfg
      case result of
        Right runner -> pure (Just runner)
        Left err     -> do
          let msg = "connector resolution failed: " <> show err
          logMsg LogWarn (T.pack msg)
          pure Nothing

-- ---------------------------------------------------------------------------
-- Terminal detection
-- ---------------------------------------------------------------------------

-- | Check if all terminal nodes have reached a terminal step state.
-- If so, set 'rcAllDone' to unblock 'waitForRun'.
checkAndSignalDone :: RunContext -> Set NodeId -> IO ()
checkAndSignalDone ctx terminals = atomically $ do
  steps <- readTVar (rcSteps ctx)
  allDone <- fmap and $
    forM (Set.toList terminals) $ \nid ->
      case Map.lookup nid steps of
        Nothing -> pure False
        Just tv -> do
          s <- readTVar tv
          pure (stepState s `elem` [StepCompleted, StepFailed, StepSkipped])
  when allDone $ writeTVar (rcAllDone ctx) True

-- ---------------------------------------------------------------------------
-- Branch isolation
-- ---------------------------------------------------------------------------

-- | Mark all executable descendants of a failed node as Skipped so the walker
-- does not block waiting for their mailboxes to fill.
-- Already-dispatched nodes and nodes already in rcSteps are untouched.
skipDescendants
  :: RunContext
  -> Graph
  -> Set NodeId
  -> NodeId
  -> AppM ()
skipDescendants ctx graph executableNodes failedNid = do
  let terminals   = Set.fromList (terminalNodes graph) `Set.intersection` executableNodes
      descendants = reachableFrom graph failedNid
      toSkip      = Set.intersection descendants executableNodes
  forM_ (Set.toList toSkip) $ \nid -> do
    alreadyDispatched <- liftIO $ atomically $
      Set.member nid <$> readTVar (rcDispatched ctx)
    alreadyHasStep <- liftIO $ atomically $
      Map.member nid <$> readTVar (rcSteps ctx)
    when (not alreadyDispatched && not alreadyHasStep) $ do
      sid <- liftIO $ StepId . UUID.toText <$> nextRandom
      now <- liftIO getCurrentTime
      let step = Step
            { stepId         = sid
            , stepRunId      = rcRunId ctx
            , stepNodeId     = nid
            , stepState      = StepPending
            , stepInput      = Nothing
            , stepOutput     = Nothing
            , stepError      = Nothing
            , stepRetryCount = 0
            , stepStartedAt  = Nothing
            , stepFinishedAt = Nothing
            }
      stepTVar <- liftIO $ do
        tv <- newTVarIO step
        atomically $ do
          modifyTVar (rcSteps ctx)      (Map.insert nid tv)
          modifyTVar (rcDispatched ctx) (Set.insert nid)
        pure tv
      insertStep step
      skipped <- transitionStep stepTVar StepSkipped now
      broadcastEvent (rcRunId ctx)
        (stepStateEvent (rcRunId ctx) nid (stepId skipped) StepSkipped now)
      liftIO $ checkAndSignalDone ctx terminals
