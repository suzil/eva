{-# LANGUAGE OverloadedStrings #-}

module Eva.App
  ( -- * Environment
    AppEnv (..)
  , DispatchFn
  , LogEntry (..)

    -- * Monad
  , AppM
  , runAppM

    -- * Startup
  , makeAppEnv

    -- * Broadcast helpers
  , broadcastEvent
  , registerRun
  , unregisterRun

    -- * Logging
  , logMsg
  , logMsgData
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson (ToJSON (..), Value (..), encode, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.IO (hFlush, stdout)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sqlite (createSqlitePool)
import qualified Data.Text.Encoding as TE
import Eva.Config (AppConfig (..), LogLevel (..))
import Eva.Core.Types
  ( Message
  , Node
  , PortName
  , ResourceBindings
  , RunId
  )
import qualified Eva.Crypto as Crypto
import Eva.Engine.LLM (LLMClient, dummyLLMClient, mkAnthropicClient, mkOpenAIClient)
import Eva.Persistence.Migration (runMigrations)

-- ---------------------------------------------------------------------------
-- Log entry
-- ---------------------------------------------------------------------------

data LogEntry = LogEntry
  { leTimestamp :: UTCTime
  , leLevel     :: LogLevel
  , leMessage   :: Text
  , leData      :: Maybe Value
  }

instance ToJSON LogEntry where
  toJSON e =
    object $
      [ "timestamp" .= leTimestamp e
      , "level" .= levelText (leLevel e)
      , "message" .= leMessage e
      ]
        ++ maybe [] (\d -> ["data" .= d]) (leData e)
    where
      levelText LogDebug = "debug" :: Text
      levelText LogInfo  = "info"
      levelText LogWarn  = "warn"
      levelText LogError = "error"

-- ---------------------------------------------------------------------------
-- Application environment
-- ---------------------------------------------------------------------------

-- | Injectable dispatch function: routes a node to its handler.
-- Production default is 'Eva.Engine.Dispatch.execute'.
-- Tests inject a custom handler (e.g. one that fails N times) via this field.
type DispatchFn =
  RunId -> Node -> Map PortName Message -> ResourceBindings -> AppM Message

data AppEnv = AppEnv
  { envConfig          :: AppConfig
  , envDbPool          :: ConnectionPool
  , envLogger          :: LogEntry -> IO ()
  , envDispatch        :: DispatchFn
  , envLLMClient       :: LLMClient
    -- ^ OpenAI client (or dummyLLMClient when EVA_LLM_API_KEY is unset).
  , envAnthropicClient :: LLMClient
    -- ^ Anthropic client (or dummyLLMClient when EVA_ANTHROPIC_API_KEY is unset).
  , envBroadcasts      :: TVar (Map RunId (TChan Value))
    -- ^ Registry of active run broadcast channels. Keyed by RunId.
    -- Engine writes events; WebSocket clients dupTChan and read.
  , envCredentialKey   :: ByteString
    -- ^ 32-byte AES-256 key derived from EVA_CREDENTIAL_KEY at startup.
  }

-- ---------------------------------------------------------------------------
-- AppM monad
-- ---------------------------------------------------------------------------

type AppM = ReaderT AppEnv IO

runAppM :: AppEnv -> AppM a -> IO a
runAppM = flip runReaderT

-- ---------------------------------------------------------------------------
-- Startup
-- ---------------------------------------------------------------------------

-- | Construct an 'AppEnv' from a loaded 'AppConfig' and an injectable dispatch
-- function. Creates the SQLite connection pool and runs auto-migration.
-- The JSON logger writes to stdout; entries below the configured log level
-- are dropped. Pass 'Eva.Engine.Dispatch.execute' as the dispatch function
-- in production; inject a custom handler in tests.
makeAppEnv :: AppConfig -> DispatchFn -> IO AppEnv
makeAppEnv cfg dispatch = do
  pool <- runNoLoggingT $
    createSqlitePool (T.pack (configDbPath cfg)) 10
  runMigrations pool
  llmClient       <- case configLlmApiKey cfg of
    Just key -> mkOpenAIClient key
    Nothing  -> pure dummyLLMClient
  anthropicClient <- case configAnthropicApiKey cfg of
    Just key -> mkAnthropicClient key
    Nothing  -> pure dummyLLMClient
  broadcasts <- newTVarIO Map.empty
  let minLevel = configLogLevel cfg
      credKey    = Crypto.deriveKey (TE.encodeUtf8 (configCredentialKey cfg))
      logger entry
        | leLevel entry < minLevel = pure ()
        | otherwise = BLC.putStrLn (encode entry) >> hFlush stdout
  pure AppEnv
    { envConfig          = cfg
    , envDbPool          = pool
    , envLogger          = logger
    , envDispatch        = dispatch
    , envLLMClient       = llmClient
    , envAnthropicClient = anthropicClient
    , envBroadcasts      = broadcasts
    , envCredentialKey   = credKey
    }

-- ---------------------------------------------------------------------------
-- Broadcast helpers
-- ---------------------------------------------------------------------------

-- | Register a run's broadcast channel so WebSocket clients can subscribe.
-- Called by the engine in 'startRun'.
registerRun :: RunId -> TChan Value -> AppM ()
registerRun rid ch = do
  broadcasts <- asks envBroadcasts
  liftIO $ atomically $ modifyTVar broadcasts (Map.insert rid ch)

-- | Remove a run's broadcast channel. Called after the run reaches a
-- terminal state so the registry doesn't grow unboundedly.
unregisterRun :: RunId -> AppM ()
unregisterRun rid = do
  broadcasts <- asks envBroadcasts
  liftIO $ atomically $ modifyTVar broadcasts (Map.delete rid)

-- | Write an event to the broadcast channel for the given run.
-- No-op if the run is not in the registry (already finished or not started).
broadcastEvent :: RunId -> Value -> AppM ()
broadcastEvent rid event = do
  broadcasts <- asks envBroadcasts
  mCh <- liftIO $ Map.lookup rid <$> readTVarIO broadcasts
  case mCh of
    Nothing -> pure ()
    Just ch -> liftIO $ atomically $ writeTChan ch event

-- ---------------------------------------------------------------------------
-- Logging helpers
-- ---------------------------------------------------------------------------

logMsg :: LogLevel -> Text -> AppM ()
logMsg level msg = logMsgData' level msg Nothing

logMsgData :: LogLevel -> Text -> Value -> AppM ()
logMsgData level msg d = logMsgData' level msg (Just d)

logMsgData' :: LogLevel -> Text -> Maybe Value -> AppM ()
logMsgData' level msg mData = do
  logger <- asks envLogger
  now    <- liftIO getCurrentTime
  liftIO $ logger LogEntry
    { leTimestamp = now
    , leLevel     = level
    , leMessage   = msg
    , leData      = mData
    }
