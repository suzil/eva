{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Eva.Config
  ( AppConfig (..)
  , LogLevel (..)
  , loadConfig
  ) where

import Data.Aeson (Value (..))
import Data.Aeson.Key (toString)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Env (AsUnread (..), Error, Parser, auto, def, help, optional, parsePure, var)
import qualified Env
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (die)

-- ---------------------------------------------------------------------------
-- Log level
-- ---------------------------------------------------------------------------

data LogLevel = LogDebug | LogInfo | LogWarn | LogError
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

-- | Custom reader for 'LogLevel' accepting lowercase names.
readLogLevel :: AsUnread e => String -> Either e LogLevel
readLogLevel s = case s of
  "debug" -> Right LogDebug
  "info"  -> Right LogInfo
  "warn"  -> Right LogWarn
  "error" -> Right LogError
  other   -> Left $ unread ("invalid log level " <> show other <> "; expected: debug, info, warn, error")

-- ---------------------------------------------------------------------------
-- AppConfig
-- ---------------------------------------------------------------------------

data AppConfig = AppConfig
  { configDbPath        :: FilePath
  , configPort          :: Int
  , configLlmApiKey     :: Maybe Text
  , configLogLevel      :: LogLevel
  , configCredentialKey :: Text
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Config parser
-- ---------------------------------------------------------------------------

configParser :: Parser Error AppConfig
configParser =
  AppConfig
    <$> var Env.str      "EVA_DB_PATH"        (def "eva.db" <> help "SQLite database file path")
    <*> var auto         "EVA_PORT"           (def 8080 <> help "HTTP server port")
    <*> optional (var Env.str "EVA_LLM_API_KEY" (help "LLM provider API key"))
    <*> var readLogLevel "EVA_LOG_LEVEL"      (def LogInfo <> help "Log level: debug, info, warn, error")
    <*> var Env.str      "EVA_CREDENTIAL_KEY" (help "Master key for credential encryption (required)")

-- ---------------------------------------------------------------------------
-- YAML fallback
-- ---------------------------------------------------------------------------

-- | Read @eva.yaml@ (if present) and return its top-level string key-value
-- pairs. Keys should match env var names, e.g. @EVA_PORT: "8080"@.
-- Returns @[]@ if the file is absent or unparseable.
loadYamlPairs :: FilePath -> IO [(String, String)]
loadYamlPairs path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      result <- Yaml.decodeFileEither path
      case result of
        Left err -> do
          putStrLn $ "Warning: could not parse " <> path <> ": " <> Yaml.prettyPrintParseException err
          pure []
        Right (Object obj) ->
          pure
            [ (toString k, T.unpack v)
            | (k, String v) <- KM.toList obj
            ]
        Right _ -> do
          putStrLn $ "Warning: " <> path <> " top level must be a YAML mapping"
          pure []

-- ---------------------------------------------------------------------------
-- Public loader
-- ---------------------------------------------------------------------------

-- | Load 'AppConfig' from environment variables, with @eva.yaml@ providing
-- defaults for any vars not set in the environment. Environment variables
-- take precedence. Exits with a clear error message on missing required config.
loadConfig :: IO AppConfig
loadConfig = do
  yamlPairs <- loadYamlPairs "eva.yaml"
  envPairs  <- getEnvironment
  -- env vars listed first so lookup finds them before yaml defaults
  let merged = envPairs ++ yamlPairs
  case parsePure configParser merged of
    Right cfg -> pure cfg
    Left errs -> die $ unlines $ "Eva configuration error:" : map (("  " <>) . showPair) errs
  where
    showPair (name, err) = name <> ": " <> show err
