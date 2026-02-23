{-# LANGUAGE OverloadedStrings #-}

-- | Knowledge node handler: resolves content into a 'knowledge_content' message.
--
-- Execution model:
--   * InlineText nodes are never dispatched by the graph walker
--     (requiredDataInputs = []) — their content reaches downstream Agents
--     statically via 'resolveResourceBindings' → 'rbKnowledge'.
--   * UpstreamPort nodes receive a message on the optional "update" data-input
--     port (requiredDataInputs = ["update"]).  The handler extracts the
--     payload, and the Runner caches it in 'rcKnowledgeCache' so that
--     downstream Agents see the fresh content via 'rbKnowledgeDynamic'.
--   * FileRef reads a local file by absolute path and returns its text content.
--   * UrlRef performs an HTTP GET and returns the response body; text/html
--     responses have tags stripped before being returned.
module Eva.Engine.Handlers.Knowledge
  ( handleKnowledge
  ) where

import Control.Exception (SomeException, catch, displayException, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Client
  ( httpLbs, parseRequest, responseBody
  , responseHeaders, responseStatus )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType, statusCode)
import System.Directory (doesFileExist)

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

    FileRef path -> do
      let p = T.unpack path
      exists <- liftIO $ doesFileExist p
      if not exists
        then liftIO $ throwIO $ userError $
               "Knowledge FileRef: file not found: " <> p
        else do
          content <- liftIO $ TIO.readFile p
          pure $ encodeContent (knowledgeFormat cfg) content

    UrlRef url -> do
      let urlStr = T.unpack url
      mgr <- liftIO newTlsManager
      req <- liftIO $
        parseRequest urlStr
          `catch` \(e :: SomeException) ->
            throwIO $ userError $
              "Knowledge UrlRef: invalid URL '" <> urlStr <> "': " <> displayException e
      response <- liftIO $ httpLbs req mgr
      let status = statusCode (responseStatus response)
          body   = responseBody response
      if status /= 200
        then liftIO $ throwIO $ userError $
               "Knowledge UrlRef: HTTP " <> show status <> " for " <> urlStr
               <> " — " <> T.unpack (TE.decodeUtf8Lenient (BL.toStrict (BL.take 200 body)))
        else do
          let rawText     = TE.decodeUtf8Lenient (BL.toStrict body)
              contentType = fromMaybe "" $ lookup hContentType (responseHeaders response)
              content     = if "html" `BS.isInfixOf` contentType
                              then stripHtmlTags rawText
                              else rawText
          pure $ encodeContent (knowledgeFormat cfg) content

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

-- | Strip HTML tags from text by tracking '<'/'>' character depth.
-- Closing '>' is replaced with a space to preserve word boundaries between
-- adjacent tag and content spans.
stripHtmlTags :: Text -> Text
stripHtmlTags t = T.strip . T.pack . reverse . snd $ T.foldl' go (False, []) t
  where
    go (_,     acc) '<' = (True,  acc)
    go (_,     acc) '>' = (False, ' ' : acc)
    go (True,  acc) _   = (True,  acc)
    go (False, acc) c   = (False, c : acc)
