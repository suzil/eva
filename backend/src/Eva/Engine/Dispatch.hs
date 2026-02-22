{-# LANGUAGE OverloadedStrings #-}

-- | Node dispatch: pattern-match on NodeType to route to the appropriate handler.
-- EVA-14: stub handlers echo inputs. EVA-24: real Agent handler.
-- EVA-25: real Action/Trigger handlers. EVA-30: connector resolution wired.
module Eva.Engine.Dispatch
  ( -- * Dispatch
    execute
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), toJSON)
import Data.Map.Strict (Map)
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)

import Eva.App (AppM)
import Eva.Core.Types  -- includes ResourceBindings
import qualified Eva.Engine.Handlers.Action    as Action
import qualified Eva.Engine.Handlers.Agent     as Agent
import qualified Eva.Engine.Handlers.Connector as Connector
import qualified Eva.Engine.Handlers.Trigger   as Trigger

-- ---------------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------------

-- | Route execution to the appropriate handler based on node type.
-- Returns the output Message that will be placed into downstream mailboxes.
execute
  :: RunId
  -> Node
  -> Map PortName Message  -- ^ Consumed data inputs (from mailboxes)
  -> ResourceBindings
  -> AppM Message
execute rid node inputs bindings = do
  now     <- liftIO getCurrentTime
  traceId <- liftIO (UUID.toText <$> nextRandom)
  let meta = MessageMeta
        { metaTraceId    = traceId
        , metaTimestamp  = now
        , metaSourceNode = nodeId node
        , metaRunId      = rid
        }
  case nodeType node of
    AgentNode     _ -> Agent.handleAgent     rid node inputs bindings
    TriggerNode   _ -> Trigger.handleTrigger rid node inputs bindings
    ActionNode    _ -> Action.handleAction   rid node inputs bindings
    KnowledgeNode _ -> pure $ Message "knowledge_content" Null meta
    -- ConnectorNode is never dispatched by the graph walker (requiredDataInputs = []).
    -- resolveConnectorRunner is called in Runner.resolveResourceBindings instead.
    -- This case is unreachable in practice but must be exhaustive.
    ConnectorNode cfg -> do
      result <- Connector.resolveConnectorRunner cfg
      case result of
        Left err ->
          pure $ Message "connector_error" (toJSON err) meta
        Right runner -> do
          specs <- liftIO $ connectorAvailableActions runner
          pure $ Message "connector_tools" (toJSON specs) meta
