{-# LANGUAGE OverloadedStrings #-}

-- | Knowledge node handler: resolves inline text (or upstream-port input) into
-- a 'knowledge_content' message.
--
-- Execution model:
--   * InlineText nodes are never dispatched by the graph walker
--     (requiredDataInputs = []) — their content reaches downstream Agents
--     statically via 'resolveResourceBindings' → 'rbKnowledge'.
--   * UpstreamPort nodes receive a message on the optional "update" data-input
--     port (requiredDataInputs = ["update"]).  The handler extracts the
--     payload, and the Runner caches it in 'rcKnowledgeCache' so that
--     downstream Agents see the fresh content via 'rbKnowledgeDynamic'.
--   * FileRef / UrlRef are not implemented in M5 and throw immediately.
module Eva.Engine.Handlers.Knowledge
  ( handleKnowledge
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)

import Eva.App (AppM)
import Eva.Core.Types

-- ---------------------------------------------------------------------------
-- Handler
-- ---------------------------------------------------------------------------

-- | Execute a Knowledge node: resolve content and emit a 'knowledge_content'
-- message.  The returned message is stored as the Step's output for
-- observability; downstream Agents receive the content via
-- 'ResourceBindings.rbKnowledgeDynamic' (for UpstreamPort) or
-- 'ResourceBindings.rbKnowledge' (for InlineText).
handleKnowledge
  :: RunId
  -> Node
  -> Map PortName Message   -- ^ Consumed data inputs ("update" for UpstreamPort)
  -> ResourceBindings
  -> AppM Message
handleKnowledge rid node inputs _bindings = do
  cfg <- case nodeType node of
    KnowledgeNode c -> pure c
    _ -> liftIO $ throwIO $ userError
           "handleKnowledge called on a non-Knowledge node"

  content <- resolveContent cfg inputs

  now     <- liftIO getCurrentTime
  traceId <- liftIO (UUID.toText <$> nextRandom)
  let meta = MessageMeta
        { metaTraceId    = traceId
        , metaTimestamp  = now
        , metaSourceNode = nodeId node
        , metaRunId      = rid
        }
  pure $ Message "knowledge_content" content meta

-- ---------------------------------------------------------------------------
-- Content resolution
-- ---------------------------------------------------------------------------

resolveContent :: KnowledgeConfig -> Map PortName Message -> AppM Value
resolveContent cfg inputs =
  case knowledgeSource cfg of
    InlineText t ->
      pure (encodeContent (knowledgeFormat cfg) t)

    UpstreamPort ->
      case Map.lookup "update" inputs of
        Nothing ->
          liftIO $ throwIO $ userError
            "Knowledge node (upstream): no message on 'update' port"
        Just msg ->
          pure (msgPayload msg)

    FileRef path ->
      liftIO $ throwIO $ userError $
        "Knowledge FileRef source not implemented in M5: " <> T.unpack path

    UrlRef url ->
      liftIO $ throwIO $ userError $
        "Knowledge UrlRef source not implemented in M5: " <> T.unpack url

-- ---------------------------------------------------------------------------
-- Format encoding
-- ---------------------------------------------------------------------------

-- | Encode inline text content according to the Knowledge node's format.
-- FormatText   → bare JSON string
-- FormatJson   → attempt to parse as JSON; fall back to JSON string on failure
-- FormatEmbedded → bare JSON string (embedding generation deferred post-MLP)
encodeContent :: KnowledgeFormat -> Text -> Value
encodeContent FormatText     t = Aeson.String t
encodeContent FormatEmbedded t = Aeson.String t
encodeContent FormatJson     t =
  case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
    Right v  -> v
    Left _   -> Aeson.String t
