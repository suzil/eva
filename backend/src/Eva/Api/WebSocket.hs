{-# LANGUAGE OverloadedStrings #-}

-- | WebSocket connection manager: topic subscriptions and event broadcast.
--
-- Clients connect to @/api/ws@ and send a subscribe message:
--
-- > { "action": "subscribe", "topic": "run:<runId>" }
-- > { "action": "subscribe", "topic": "assistant:<conversationId>" }
--
-- === Run topics (@run:\<runId\>@)
--
-- The server streams all events for that run until the run reaches a
-- terminal state ('run_state' with completed/failed/canceled), at which point
-- it closes the connection gracefully.
--
-- === Assistant topics (@assistant:\<conversationId\>@)
--
-- The connection is bidirectional. After subscribing, the client sends
-- @assistant_message@ actions and receives streamed @assistant_token@ events
-- followed by an @assistant_reply@ event for each message.
--
-- > { "action": "assistant_message", "content": "...", "context": { ... } }
--
-- Event shapes:
--
-- * @step_state@        — node step transitioned (pending→running→completed/failed/skipped)
-- * @run_state@         — run transitioned (running→completed/failed/canceled)
-- * @llm_token@         — individual LLM streaming token from an Agent node
-- * @log_entry@         — retry warning or structured log from the engine
-- * @tool_call@         — LLM invoked a connector tool (tool name, args, result)
-- * @code_change_event@ — SystemCodebase connector staged a code changeset
-- * @assistant_token@   — individual streaming token from the MAGI assistant
-- * @assistant_reply@   — final 'AssistantMessage' from the MAGI assistant
module Eva.Api.WebSocket
  ( -- * WAI server app
    wsServerApp

    -- * Event constructors (used by engine and handlers)
  , stepStateEvent
  , runStateEvent
  , llmTokenEvent
  , logEntryEvent
  , toolCallEvent
  , codeChangeEvent
  , assistantTokenEvent
  , assistantReplyEvent

    -- * Exported for testing
  , SubscribeMsg (..)
  , Topic (..)
  , parseTopic
  , parseRunIdFromTopic
  , isTerminalRunState
  ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Exception (Handler (..), IOException, SomeException, catches, handle, try)
import Control.Monad (forever)
import System.IO (hPutStrLn, stderr)
import Data.Aeson (FromJSON (..), Value, decode, encode, object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import qualified Network.WebSockets as WS

import Eva.App (AppEnv (..), runAppM)
import Eva.Assistant
  ( AssistantContext
  , AssistantMessage (..)
  , ConversationId (..)
  , handleAssistantMessage
  )
import Eva.Codebase.Types (CodeChangesetId)
import Eva.Core.Types (NodeId, Run (..), RunId (..), RunState (..), StepId, StepState (..))
import Eva.Persistence.Queries (getRun)

-- ---------------------------------------------------------------------------
-- Topic type
-- ---------------------------------------------------------------------------

-- | Parsed topic from a 'SubscribeMsg'.
data Topic
  = RunTopic       RunId
  | AssistantTopic ConversationId
  deriving (Eq, Show)

-- | Parse a topic string into a 'Topic'.
-- Returns 'Nothing' for unrecognised prefixes or empty IDs.
parseTopic :: Text -> Maybe Topic
parseTopic t
  | Just rid <- T.stripPrefix "run:"       t, not (T.null rid) = Just (RunTopic (RunId rid))
  | Just cid <- T.stripPrefix "assistant:" t, not (T.null cid) = Just (AssistantTopic (ConversationId cid))
  | otherwise = Nothing

-- | Backwards-compatible helper: extract a 'RunId' from a @"run:<id>"@ topic.
parseRunIdFromTopic :: Text -> Maybe RunId
parseRunIdFromTopic t = case parseTopic t of
  Just (RunTopic rid) -> Just rid
  _                   -> Nothing

-- ---------------------------------------------------------------------------
-- Subscribe message
-- ---------------------------------------------------------------------------

-- | Message sent by the client immediately after connecting.
data SubscribeMsg = SubscribeMsg
  { subAction :: Text   -- ^ Expected to be "subscribe"
  , subTopic  :: Text   -- ^ Expected format: "run:<runId>" or "assistant:<conversationId>"
  }

instance FromJSON SubscribeMsg where
  parseJSON = withObject "SubscribeMsg" $ \o ->
    SubscribeMsg <$> o .: "action" <*> o .: "topic"

-- ---------------------------------------------------------------------------
-- Assistant client message
-- ---------------------------------------------------------------------------

-- | Message sent by the client on an assistant topic connection.
-- > { "action": "assistant_message", "content": "...", "context": { ... } }
data AssistantClientMsg = AssistantClientMsg
  { acmContent :: Text
  , acmContext :: AssistantContext
  }

instance FromJSON AssistantClientMsg where
  parseJSON = withObject "AssistantClientMsg" $ \o ->
    AssistantClientMsg
      <$> o .: "content"
      <*> o .: "context"

-- ---------------------------------------------------------------------------
-- WAI / WebSocket server app
-- ---------------------------------------------------------------------------

-- | WebSocket server app to be composed with 'Network.Wai.Handler.WebSockets.websocketsOr'.
-- Accepts any WebSocket upgrade, then expects a subscribe message.
wsServerApp :: AppEnv -> WS.ServerApp
wsServerApp env pendingConn = do
  conn <- WS.acceptRequest pendingConn
  WS.withPingThread conn 30 (pure ()) $
    handleClient env conn

handleClient :: AppEnv -> WS.Connection -> IO ()
handleClient env conn =
  handle (\(_ :: WS.ConnectionException) -> pure ()) $ do
    rawMsg <- WS.receiveData conn
    case decode rawMsg of
      Nothing -> sendError conn "invalid subscribe message"
      Just (SubscribeMsg action topic)
        | action /= "subscribe" -> sendError conn "expected action: subscribe"
        | otherwise ->
            case parseTopic topic of
              Nothing                    -> sendError conn "invalid topic format; expected run:<id> or assistant:<id>"
              Just (RunTopic rid)        -> subscribeAndForward env conn rid
              Just (AssistantTopic cid)  -> subscribeAssistant  env conn cid

-- ---------------------------------------------------------------------------
-- Run topic: subscribe and forward
-- ---------------------------------------------------------------------------

-- | Look up the broadcast channel for the given run, dup it, and forward
-- events to the client until the run reaches a terminal state or disconnects.
-- If the run already finished before the client subscribed, fetch its state
-- from DB and emit a synthetic terminal event so the client can update.
--
-- The read + dupTChan is done in a single STM transaction so that it is
-- atomic with respect to 'broadcastAndUnregisterRun': either we dup before
-- the terminal write (and will see it), or we find Nothing in the map (and
-- fall through to the synthetic path). Without this atomicity, we could dup
-- after the terminal write but before the map deletion, starting the read
-- cursor after the terminal event and blocking forwardEvents forever.
subscribeAndForward :: AppEnv -> WS.Connection -> RunId -> IO ()
subscribeAndForward env conn rid = do
  mDupCh <- atomically $ do
    bMap <- readTVar (envBroadcasts env)
    case Map.lookup rid bMap of
      Just ch -> Just <$> dupTChan ch
      Nothing -> pure Nothing
  case mDupCh of
    Just dupCh -> forwardRunEvents conn dupCh
    Nothing -> do
      -- Run finished before the client subscribed — look up its final state
      -- and send a synthetic terminal event so the frontend can update.
      mRun <- runAppM env (getRun rid)
      now  <- getCurrentTime
      case mRun of
        Nothing  -> sendError conn "run not found"
        Just run -> WS.sendTextData conn
                      (encode (runStateEvent rid (runState run) now))

-- | Read events from the duplicated channel and send them to the client.
-- Closes cleanly when:
--   * A terminal run_state event is received.
--   * The client disconnects (WS.ConnectionException is caught).
--
-- Token events (llm_token) are batched within a 50ms window: instead of one
-- WS frame per token, all tokens arriving within the window are concatenated
-- into a single event. Non-token events flush the buffer immediately.
forwardRunEvents :: WS.Connection -> TChan Value -> IO ()
forwardRunEvents conn ch =
  catches (go [])
    [ Handler (\(_ :: WS.ConnectionException) -> pure ())
    , Handler (\(_ :: IOException)            -> pure ())
    ]
  where
    go buf = do
      timer <- registerDelay 50000          -- 50ms batching window
      mEv   <- atomically $
        (Just <$> readTChan ch)
          `orElse`
          (readTVar timer >>= \t -> if t then pure Nothing else retry)
      case mEv of
        Nothing ->                          -- timer fired: flush token buffer
          flushBuf buf >> go []
        Just ev
          | isTokenEvent ev       -> go (ev : buf)
          | isTerminalRunState ev -> flushBuf buf >> send ev
          | otherwise             -> flushBuf buf >> send ev >> go []
    flushBuf []  = pure ()
    flushBuf buf = send (batchTokenEvents (reverse buf))
    send ev      = WS.sendTextData conn (encode ev)

-- | Check whether a broadcast event indicates the run has reached a terminal
-- state (completed, failed, or canceled). Used to close the WS connection.
isTerminalRunState :: Value -> Bool
isTerminalRunState (Aeson.Object o) =
  case (KM.lookup "type" o, KM.lookup "state" o) of
    (Just (Aeson.String "run_state"), Just (Aeson.String s)) ->
      s `elem` ["completed", "failed", "canceled"]
    _ -> False
isTerminalRunState _ = False

-- ---------------------------------------------------------------------------
-- Assistant topic: bidirectional subscribe
-- ---------------------------------------------------------------------------

-- | Handle an @assistant:<conversationId>@ subscription.
--
-- Creates a fresh broadcast channel for the conversation, registers it in
-- 'envAssistantBroadcasts', then:
--
-- * Forks 'forwardAssistantEvents' to stream server→client events.
-- * Loops on the main thread reading @assistant_message@ actions from the
--   client, calling 'handleAssistantMessage' for each, and broadcasting
--   'assistantTokenEvent' / 'assistantReplyEvent' to the channel.
--
-- When the client disconnects, 'withAsync' cancels the forwarding thread and
-- the conversation is removed from the registry.
subscribeAssistant :: AppEnv -> WS.Connection -> ConversationId -> IO ()
subscribeAssistant env conn cid@(ConversationId cidText) = do
  hPutStrLn stderr ("[MAGI] subscribeAssistant: " <> T.unpack cidText)
  ch <- newTChanIO
  atomically $ modifyTVar (envAssistantBroadcasts env) (Map.insert cidText ch)
  dupCh <- atomically $ dupTChan ch
  handle (\(e :: SomeException) -> hPutStrLn stderr ("[MAGI] outer exception: " <> show e)) $
    withAsync (forwardAssistantEvents conn dupCh) $ \_fwd ->
      handle (\(_ :: WS.ConnectionException) -> pure ()) (clientLoop ch)
  hPutStrLn stderr ("[MAGI] subscribeAssistant done: " <> T.unpack cidText)
  atomically $ modifyTVar (envAssistantBroadcasts env) (Map.delete cidText)
  where
    clientLoop ch = forever $ do
      rawMsg <- WS.receiveData conn
      hPutStrLn stderr "[MAGI] received message"
      case decode rawMsg of
        Nothing  -> do
          hPutStrLn stderr "[MAGI] decode failed"
          sendError conn "invalid message format"
        Just msg -> do
          hPutStrLn stderr ("[MAGI] handling: " <> T.unpack (acmContent msg))
          let onToken tok = do
                ts <- getCurrentTime
                atomically $ writeTChan ch (assistantTokenEvent cid tok ts)
          eResult <- try $ runAppM env $
            handleAssistantMessage cid (acmContent msg) (acmContext msg) onToken
          ts <- getCurrentTime
          let reply = case (eResult :: Either SomeException AssistantMessage) of
                Left  err -> AsstText ("MAGI error: " <> T.pack (show err))
                Right r   -> r
          case (eResult :: Either SomeException AssistantMessage) of
            Left err -> hPutStrLn stderr ("[MAGI] exception: " <> show err)
            Right _  -> hPutStrLn stderr "[MAGI] reply ready"
          atomically $ writeTChan ch (assistantReplyEvent cid reply ts)

-- | Forward events from a conversation channel to the WebSocket client
-- indefinitely. Exits cleanly on disconnect.
--
-- Token events (assistant_token) are batched within a 50ms window to reduce
-- WS frame count during LLM streaming. Non-token events flush immediately.
forwardAssistantEvents :: WS.Connection -> TChan Value -> IO ()
forwardAssistantEvents conn ch =
  catches (go [])
    [ Handler (\(_ :: WS.ConnectionException) -> pure ())
    , Handler (\(_ :: IOException)            -> pure ())
    ]
  where
    go buf = do
      timer <- registerDelay 50000
      mEv   <- atomically $
        (Just <$> readTChan ch)
          `orElse`
          (readTVar timer >>= \t -> if t then pure Nothing else retry)
      case mEv of
        Nothing -> flushBuf buf >> go []
        Just ev
          | isTokenEvent ev -> go (ev : buf)
          | otherwise        -> flushBuf buf >> send ev >> go []
    flushBuf []  = pure ()
    flushBuf buf = send (batchTokenEvents (reverse buf))
    send ev      = WS.sendTextData conn (encode ev)

-- ---------------------------------------------------------------------------
-- Token batching helpers
-- ---------------------------------------------------------------------------

-- | True for llm_token and assistant_token events.
isTokenEvent :: Value -> Bool
isTokenEvent (Aeson.Object o) = case KM.lookup "type" o of
  Just (Aeson.String t) -> t `elem` ["llm_token", "assistant_token"]
  _                     -> False
isTokenEvent _ = False

-- | Merge a chronological list of token events into one event, concatenating
-- all \"token\" field values. The first event's metadata (type, runId /
-- conversationId, nodeId, timestamp) is preserved unchanged.
batchTokenEvents :: [Value] -> Value
batchTokenEvents []           = Aeson.Null
batchTokenEvents evs@(first : _) =
  let tokens = [ tok
               | Aeson.Object o <- evs
               , Just (Aeson.String tok) <- [KM.lookup "token" o]
               ]
  in case first of
       Aeson.Object o -> Aeson.Object (KM.insert "token" (Aeson.String (mconcat tokens)) o)
       _              -> first

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Send a JSON error message to the client.
sendError :: WS.Connection -> Text -> IO ()
sendError conn msg =
  WS.sendTextData conn (encode (object ["error" .= msg]))

-- ---------------------------------------------------------------------------
-- Event constructors
-- ---------------------------------------------------------------------------

-- | Build a @step_state@ event for broadcast.
stepStateEvent :: RunId -> NodeId -> StepId -> StepState -> UTCTime -> Value
stepStateEvent rid nid sid state ts =
  object
    [ "type"      .= ("step_state" :: Text)
    , "runId"     .= rid
    , "nodeId"    .= nid
    , "stepId"    .= sid
    , "state"     .= state
    , "timestamp" .= ts
    ]

-- | Build a @run_state@ event for broadcast.
runStateEvent :: RunId -> RunState -> UTCTime -> Value
runStateEvent rid state ts =
  object
    [ "type"      .= ("run_state" :: Text)
    , "runId"     .= rid
    , "state"     .= state
    , "timestamp" .= ts
    ]

-- | Build an @llm_token@ event for broadcast.
llmTokenEvent :: RunId -> NodeId -> Text -> UTCTime -> Value
llmTokenEvent rid nid token ts =
  object
    [ "type"      .= ("llm_token" :: Text)
    , "runId"     .= rid
    , "nodeId"    .= nid
    , "token"     .= token
    , "timestamp" .= ts
    ]

-- | Build a @log_entry@ event for broadcast.
logEntryEvent :: RunId -> StepId -> Text -> Text -> UTCTime -> Value
logEntryEvent rid sid level msg ts =
  object
    [ "type"      .= ("log_entry" :: Text)
    , "runId"     .= rid
    , "stepId"    .= sid
    , "level"     .= level
    , "message"   .= msg
    , "timestamp" .= ts
    ]

-- | Build a @tool_call@ event for broadcast.
-- Emitted once per tool call and once per tool result during an Agent run.
toolCallEvent :: RunId -> NodeId -> Text -> Value -> UTCTime -> Value
toolCallEvent rid nid phase payload ts =
  object
    [ "type"      .= ("tool_call" :: Text)
    , "runId"     .= rid
    , "nodeId"    .= nid
    , "phase"     .= phase   -- "invoke" or "result"
    , "data"      .= payload
    , "timestamp" .= ts
    ]

-- | Build a @code_change_event@ for broadcast.
-- Emitted by the SystemCodebase connector handler after staging a write
-- operation as a pending CodeChangeset.
codeChangeEvent :: RunId -> CodeChangesetId -> Int -> UTCTime -> Value
codeChangeEvent rid csId fileCount ts =
  object
    [ "type"        .= ("code_change_event" :: Text)
    , "runId"       .= rid
    , "changesetId" .= csId
    , "fileCount"   .= fileCount
    , "timestamp"   .= ts
    ]

-- | Build an @assistant_token@ event for broadcast on an assistant topic.
-- Emitted once per streaming token during a MAGI response.
assistantTokenEvent :: ConversationId -> Text -> UTCTime -> Value
assistantTokenEvent cid token ts =
  object
    [ "type"           .= ("assistant_token" :: Text)
    , "conversationId" .= cid
    , "token"          .= token
    , "timestamp"      .= ts
    ]

-- | Build an @assistant_reply@ event for broadcast on an assistant topic.
-- Emitted once per user message after the full response is assembled.
assistantReplyEvent :: ConversationId -> AssistantMessage -> UTCTime -> Value
assistantReplyEvent cid msg ts =
  object
    [ "type"           .= ("assistant_reply" :: Text)
    , "conversationId" .= cid
    , "message"        .= msg
    , "timestamp"      .= ts
    ]
