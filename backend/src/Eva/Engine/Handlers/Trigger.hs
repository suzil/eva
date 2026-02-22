{-# LANGUAGE OverloadedStrings #-}

-- | Trigger node handler: emits an event message to start graph execution.
-- Only 'TriggerManual' is implemented in M4. Cron trigger is EVA-35.
module Eva.Engine.Handlers.Trigger
  ( handleTrigger
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)

import Eva.App (AppM)
import Eva.Core.Types
import Eva.Persistence.Queries (getRun)

-- ---------------------------------------------------------------------------
-- Handler
-- ---------------------------------------------------------------------------

-- | Execute a Trigger node: emit an 'event' message carrying the run's
-- trigger payload. Called by the runner with an empty inputs map since the
-- trigger is the graph entry point.
handleTrigger
  :: RunId
  -> Node
  -> Map PortName Message   -- ^ Always empty for trigger nodes
  -> ResourceBindings
  -> AppM Message
handleTrigger rid node _inputs _bindings = do
  -- 1. Extract TriggerConfig from the node.
  cfg <- case nodeType node of
    TriggerNode c -> pure c
    _             -> liftIO $ throwIO $ userError "handleTrigger called on a non-Trigger node"

  -- 2. Dispatch on trigger type.
  case triggerType cfg of
    TriggerManual -> handleManual rid node cfg
    TriggerCron   -> handleManual rid node cfg  -- fires same event message; scheduler sets no payload
    other         -> liftIO $ throwIO $ userError $
      "Trigger type " <> show other <> " not yet implemented"

-- ---------------------------------------------------------------------------
-- Manual trigger
-- ---------------------------------------------------------------------------

handleManual :: RunId -> Node -> TriggerConfig -> AppM Message
handleManual rid node cfg = do
  -- 1. Determine the event payload.
  --    Priority: triggerPayloadTemplate (static config) > run's triggerInfo > {}
  payload <- case triggerPayloadTemplate cfg of
    Just tmpl -> pure tmpl
    Nothing   -> do
      mRun <- getRun rid
      pure $ fromMaybe (Aeson.object []) (mRun >>= runTriggerInfo)

  -- 2. Build and return the output message.
  now     <- liftIO getCurrentTime
  traceId <- liftIO (UUID.toText <$> nextRandom)
  let meta = MessageMeta
        { metaTraceId    = traceId
        , metaTimestamp  = now
        , metaSourceNode = nodeId node
        , metaRunId      = rid
        }
  pure $ Message "event" payload meta
