{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Execution engine: RunContext creation, graph walker, message propagation,
-- terminal detection. Uses STM + async for concurrent branch execution.
--
-- Execution model:
--   1. 'startRun' creates a RunContext with per-port TMVar mailboxes, transitions
--      the Run to Running, executes Trigger nodes synchronously, then forks the
--      graph walker as a background async.
--   2. The graph walker loops via STM: atomically finds nodes whose required data
--      inputs are all filled, consumes those messages, forks an async per ready
--      node, waits for the batch, then repeats.
--   3. 'executeNodeStep' creates/transitions a Step, calls Dispatch.execute,
--      places the output into downstream mailboxes, and signals done when all
--      terminal nodes have finished.
--   4. Resource edges are resolved as static context at dispatch time — they do
--      not get mailboxes and do not gate execution readiness.
module Eva.Engine.Runner
  ( -- * Run lifecycle
    startRun
  , waitForRun

    -- * Exposed for testing
  , resolveResourceBindings
  ) where

import Control.Concurrent.Async (async, waitCatch)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (Value, toJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)

import Eva.App (AppEnv, AppM, runAppM)
import Eva.Core.Graph
  ( dataEdgesOf
  , requiredDataInputs
  , rootNodes
  , terminalNodes
  )
import Eva.Core.Types
import Eva.Engine.Dispatch (ResourceBindings (..), execute)
import Eva.Engine.StateMachine
  ( RunContext (..)
  , newRunContext
  , transitionRun
  , transitionStep
  , waitForRun
  )
import Eva.Persistence.Queries (insertRun, insertStep)

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

  -- 1. Allocate identifiers and build the initial Run record.
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

  -- 2. Compute mailbox keys: one per required data-input port across all nodes.
  let dataPorts =
        [ (nid, port)
        | (nid, node) <- Map.toList (graphNodes graph)
        , port <- requiredDataInputs (nodeType node)
        ]

  -- 3. Create the RunContext (allocates TMVars, TVars, broadcast channel).
  ctx <- liftIO $ newRunContext run dataPorts

  -- 4. Persist the Run and transition pending → running.
  insertRun run
  _ <- transitionRun (rcRun ctx) RunRunning now

  -- 5. Execute Trigger nodes (root nodes with no data inputs) synchronously.
  --    Their output messages seed the downstream mailboxes before the walker starts.
  let dEdges = dataEdgesOf graph
  forM_ (rootNodes graph) $ \nid ->
    case Map.lookup nid (graphNodes graph) of
      Nothing   -> pure ()
      Just node -> case nodeType node of
        TriggerNode _ -> do
          outMsg <- executeNodeStep env ctx graph node Map.empty
          liftIO $ placeMessages ctx dEdges nid outMsg
        _ -> pure ()

  -- 6. Fork the graph walker as a background thread.
  _ <- liftIO $ async $ runAppM env $ graphWalkerLoop ctx graph

  pure ctx

-- ---------------------------------------------------------------------------
-- Graph walker
-- ---------------------------------------------------------------------------

-- | Main graph-walking loop. Runs in a background async thread.
-- Each iteration atomically finds ready nodes, forks their execution in
-- parallel, waits for the batch, then loops. Exits when all terminal nodes
-- are complete and 'rcAllDone' is set.
graphWalkerLoop :: RunContext -> Graph -> AppM ()
graphWalkerLoop ctx graph = do
  env <- ask
  let dEdges = dataEdgesOf graph
      -- "Executable" nodes are those that will have Steps created during this
      -- run. Steps are created for:
      --   • TriggerNode — executed synchronously in startRun
      --   • Any node with at least one required data input — executed by the
      --     walker when its mailboxes fill (Agent, Action in MLP)
      -- Knowledge and Connector nodes have no required data inputs and are not
      -- TriggerNodes, so they never receive steps and must NOT be terminals.
      -- Including them would cause checkAndSignalDone to never fire, deadlocking
      -- the walker.
      willHaveStep (_nid, node) = case nodeType node of
        TriggerNode _ -> True
        _             -> not (null (requiredDataInputs (nodeType node)))
      executableNodes =
        Set.fromList
          [ nid | (nid, node) <- Map.toList (graphNodes graph), willHaveStep (nid, node) ]
      terminals = Set.fromList (terminalNodes graph) `Set.intersection` executableNodes
  go env dEdges terminals
  where
    go env dEdges terminals = do
      -- STM: atomically collect ready nodes and consume their input messages.
      -- If rcAllDone is set (terminal detection fired), return [] to exit.
      -- If nothing is ready yet, STM retries (blocking) until a TMVar changes.
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

      -- When readyInputs is [] the walker should exit (rcAllDone was True).
      -- This check prevents an infinite loop if collectReadyInputs somehow
      -- returns [] without the retry path being taken.
      if null readyInputs
        then finishRun
        else do
          -- Fork every ready node in parallel.
          asyncs <- forM readyInputs $ \(nid, inputs) ->
            liftIO $ async $ runAppM env $ do
              case Map.lookup nid (graphNodes graph) of
                Nothing   -> pure ()
                Just node -> do
                  outMsg <- executeNodeStep env ctx graph node inputs
                  liftIO $ placeMessages ctx dEdges nid outMsg
                  liftIO $ checkAndSignalDone ctx terminals

          -- Wait for all forks; on any failure, transition Run to Failed and stop.
          results <- liftIO $ mapM waitCatch asyncs
          let failures = [e | Left e <- results] :: [SomeException]
          if null failures
            then go env dEdges terminals
            else do
              now <- liftIO getCurrentTime
              _ <- transitionRun (rcRun ctx) RunFailed now
              liftIO $ atomically $ writeTVar (rcAllDone ctx) True

    -- Transition the Run to Completed if it is still in Running state.
    finishRun = do
      run <- liftIO $ readTVarIO (rcRun ctx)
      case runState run of
        RunRunning -> do
          now <- liftIO getCurrentTime
          _ <- transitionRun (rcRun ctx) RunCompleted now
          pure ()
        _ -> pure ()

-- ---------------------------------------------------------------------------
-- Node step execution
-- ---------------------------------------------------------------------------

-- | Execute a single node within a run: allocate a Step, transition through
-- Pending→Running→Completed (or Failed on exception), and return the output.
executeNodeStep
  :: AppEnv
  -> RunContext
  -> Graph
  -> Node
  -> Map PortName Message  -- ^ Consumed data inputs
  -> AppM Message
executeNodeStep env ctx graph node inputs = do
  -- 1. Allocate and persist the Step in Pending state.
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

  -- 2. Transition Pending → Running.
  _ <- transitionStep stepTVar StepRunning now

  -- 3. Resolve resource bindings (pure graph lookup — no mailboxes involved).
  let bindings = resolveResourceBindings graph (nodeId node)

  -- 4. Dispatch to the appropriate handler.
  result <- liftIO $ try $ runAppM env $ execute (rcRunId ctx) node inputs bindings

  case result of
    Right outMsg -> do
      -- 5a. Record output, transition Running → Completed.
      now2 <- liftIO getCurrentTime
      liftIO $ atomically $
        modifyTVar stepTVar (\s -> s { stepOutput = Just (toJSON outMsg) })
      _ <- transitionStep stepTVar StepCompleted now2
      pure outMsg

    Left (e :: SomeException) -> do
      -- 5b. Record error, transition Running → Failed, re-throw.
      now2 <- liftIO getCurrentTime
      liftIO $ atomically $
        modifyTVar stepTVar (\s -> s { stepError = Just (T.pack (show e)) })
      _ <- transitionStep stepTVar StepFailed now2
      liftIO $ ioError (userError (show e))

-- ---------------------------------------------------------------------------
-- Mailbox helpers
-- ---------------------------------------------------------------------------

-- | Place a message into the downstream mailboxes of all data edges that
-- originate from @srcNode@. Silently skips any edge whose target port
-- does not have a mailbox (e.g. resource ports, or unmapped ports).
placeMessages :: RunContext -> [Edge] -> NodeId -> Message -> IO ()
placeMessages ctx dEdges srcNode msg =
  atomically $
    forM_ dEdges $ \e ->
      when (edgeSourceNode e == srcNode) $
        case Map.lookup (edgeTargetNode e, edgeTargetPort e) (rcMailboxes ctx) of
          Just tmv -> putTMVar tmv msg
          Nothing  -> pure ()

-- | STM action: scan all un-dispatched nodes and return those whose required
-- data-input mailboxes are all full. Atomically consumes (takes) the messages
-- for every node included in the result.
--
-- Returns [] if no nodes are currently ready. The caller should call 'retry'
-- when it wants to block until something becomes available.
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
            -- Nodes with no required data inputs (Trigger, Knowledge, Connector)
            -- are handled as roots/statics, not by the readiness loop.
            [] -> pure Nothing
            _  -> do
              -- Peek (non-blocking) to check all required ports are filled.
              peeks <- forM required $ \port ->
                case Map.lookup (nid, port) (rcMailboxes ctx) of
                  Nothing  -> pure Nothing
                  Just tmv -> tryReadTMVar tmv
              if all (/= Nothing) peeks
                then do
                  -- All required ports are full: consume (take) each message.
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

-- | Resolve the static resource bindings for @nid@: scan all resource edges
-- whose target is @nid@, look up each source node, and collect Knowledge/
-- Connector configs. This is a pure graph traversal — no runtime state.
resolveResourceBindings :: Graph -> NodeId -> ResourceBindings
resolveResourceBindings graph nid =
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
  in ResourceBindings
       { rbKnowledge  = knowledge
       , rbConnectors = connectors
       }

-- ---------------------------------------------------------------------------
-- Terminal detection
-- ---------------------------------------------------------------------------

-- | After a step completes, check if all terminal nodes (no outgoing data edges)
-- are in a terminal step state. If so, set 'rcAllDone' to unblock 'waitForRun'
-- and let the walker exit.
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
