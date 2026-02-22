{-# LANGUAGE OverloadedStrings #-}

-- | Node dispatch: pattern-match on NodeType to route to the appropriate handler.
-- EVA-14 provides stub handlers that echo inputs; real implementations follow in
-- EVA-23 (LLM client), EVA-24 (Agent handler), EVA-25 (Action/Trigger handlers).
module Eva.Engine.Dispatch
  ( -- * Resource bindings
    ResourceBindings (..)

    -- * Dispatch
  , execute
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)

import Eva.App (AppM)
import Eva.Core.Types

-- ---------------------------------------------------------------------------
-- Resource bindings
-- ---------------------------------------------------------------------------

-- | Resolved resource connections for a node: Knowledge content sources and
-- Connector capability configs. Computed from resource edges at dispatch time.
-- Resource ports are static grants — they do not carry runtime messages.
data ResourceBindings = ResourceBindings
  { rbKnowledge  :: [KnowledgeConfig]
    -- ^ Config of each Knowledge node connected via a resource edge.
  , rbConnectors :: [ConnectorConfig]
    -- ^ Config of each Connector node connected via a resource edge.
  }

-- ---------------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------------

-- | Route execution to the appropriate handler based on node type.
-- Returns the output Message that will be placed into downstream mailboxes.
--
-- EVA-14: all handlers are stubs that echo the first data input payload
-- (or emit an empty object for nodes with no data inputs).
-- Real handlers are implemented in EVA-23/24/25.
execute
  :: RunId
  -> Node
  -> Map PortName Message  -- ^ Consumed data inputs (from mailboxes)
  -> ResourceBindings
  -> AppM Message
execute rid node inputs _bindings = do
  now     <- liftIO getCurrentTime
  traceId <- liftIO (UUID.toText <$> nextRandom)
  let meta = MessageMeta
        { metaTraceId   = traceId
        , metaTimestamp = now
        , metaSourceNode = nodeId node
        , metaRunId     = rid
        }
      firstPayload = firstInputPayload inputs
  pure $ case nodeType node of
    TriggerNode   _ -> Message "event"             firstPayload meta
    ActionNode    _ -> Message "action_output"     firstPayload meta
    AgentNode     _ -> Message "agent_output"      firstPayload meta
    KnowledgeNode _ -> Message "knowledge_content" Null         meta
    ConnectorNode _ -> Message "connector_response" Null        meta

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Return the payload of the first message in the inputs map, or Null.
firstInputPayload :: Map PortName Message -> Value
firstInputPayload m =
  case Map.elems m of
    (msg : _) -> msgPayload msg
    []        -> Aeson.object []
