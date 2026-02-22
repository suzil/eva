{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for EVA-30: Connector framework.
-- Tests resolveConnectorRunner in isolation: missing credential, invalid
-- credential (bad decrypt), unsupported system, and a valid Linear stub.
module Eva.Engine.Handlers.ConnectorSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Map.Strict as Map
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import Eva.App (AppEnv (..), runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.Handlers.Connector (resolveConnectorRunner)
import Eva.Engine.LLM (LLMClient (..), LLMResponse (..), TokenUsage (..), dummyLLMClient)
import Eva.Integration.Types
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries (insertCredential)

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

withTestEnv :: (AppEnv -> IO ()) -> IO ()
withTestEnv action = do
  pool       <- runNoLoggingT $ createSqlitePool ":memory:" 2
  runMigrations pool
  broadcasts <- newTVarIO Map.empty
  let cfg = AppConfig
        { configDbPath        = ":memory:"
        , configPort          = 8080
        , configLlmApiKey       = Nothing
        , configAnthropicApiKey = Nothing
        , configLogLevel        = LogError
        , configCredentialKey   = "test-credential-key"
        , configStaticDir       = Nothing
        }
      env = AppEnv
        { envConfig          = cfg
        , envDbPool          = pool
        , envLogger          = \_ -> pure ()
        , envDispatch        = \_ _ _ _ -> error "dispatch not used in connector tests"
        , envLLMClient       = dummyLLMClient
        , envAnthropicClient = dummyLLMClient
        , envBroadcasts      = broadcasts
        , envCredentialKey   = Crypto.deriveKey "test-credential-key"
        }
  action env

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

testCreatedAt :: UTCTime
testCreatedAt = read "2026-01-01 00:00:00 UTC"

-- | A ConnectorConfig pointing at a credential that does not exist in the DB.
missingCredConfig :: ConnectorConfig
missingCredConfig = ConnectorConfig
  { connectorSystem       = SystemLinear
  , connectorCredentialId = Just "cred-does-not-exist"
  , connectorEndpoint     = Nothing
  , connectorScope        = Nothing
  , connectorActionFilter = []
  }

-- | A ConnectorConfig with no credential ID set.
noCredConfig :: ConnectorConfig
noCredConfig = ConnectorConfig
  { connectorSystem       = SystemLinear
  , connectorCredentialId = Nothing
  , connectorEndpoint     = Nothing
  , connectorScope        = Nothing
  , connectorActionFilter = []
  }

-- | A ConnectorConfig for a system not yet implemented (HTTP).
unsupportedConfig :: ConnectorConfig
unsupportedConfig = ConnectorConfig
  { connectorSystem       = SystemHttp
  , connectorCredentialId = Just "cred-http"
  , connectorEndpoint     = Nothing
  , connectorScope        = Nothing
  , connectorActionFilter = []
  }

-- | A ConnectorConfig for Linear with a valid credential.
linearConfig :: ConnectorConfig
linearConfig = ConnectorConfig
  { connectorSystem       = SystemLinear
  , connectorCredentialId = Just "cred-linear"
  , connectorEndpoint     = Nothing
  , connectorScope        = Nothing
  , connectorActionFilter = []
  }

-- | Insert a test Linear credential encrypted with the test key.
insertLinearCredential :: AppEnv -> IO ()
insertLinearCredential env = do
  let cred = Credential
        { credentialId        = CredentialId "cred-linear"
        , credentialName      = "Linear test token"
        , credentialSystem    = SystemLinear
        , credentialType      = CredentialApiKey
        , credentialCreatedAt = testCreatedAt
        }
      secret    = TE.encodeUtf8 "lin_api_test_token_123"
      credKey   = envCredentialKey env
  encBytes <- Crypto.encrypt credKey secret
  runAppM env $ insertCredential cred encBytes

-- | Insert a credential with bytes encrypted under the WRONG key (invalid blob).
insertCorruptCredential :: AppEnv -> IO ()
insertCorruptCredential env = do
  let cred = Credential
        { credentialId        = CredentialId "cred-corrupt"
        , credentialName      = "Corrupt credential"
        , credentialSystem    = SystemLinear
        , credentialType      = CredentialApiKey
        , credentialCreatedAt = testCreatedAt
        }
  encBytes <- Crypto.encrypt (Crypto.deriveKey "wrong-key") (TE.encodeUtf8 "secret")
  runAppM env $ insertCredential cred encBytes

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "resolveConnectorRunner" $ do

    it "returns ConnectorMissingCredential when credentialId is Nothing" $
      withTestEnv $ \env -> do
        result <- runAppM env $ resolveConnectorRunner noCredConfig
        case result of
          Left (ConnectorMissingCredential _) -> pure ()
          Left other  -> expectationFailure $
            "expected ConnectorMissingCredential, got: " <> T.unpack (connectorErrorText other)
          Right _ -> expectationFailure "expected Left, got Right runner"

    it "returns ConnectorInvalidCredential when credential not found in DB" $
      withTestEnv $ \env -> do
        result <- runAppM env $ resolveConnectorRunner missingCredConfig
        case result of
          Left (ConnectorInvalidCredential msg) ->
            T.unpack msg `shouldContain` "cred-does-not-exist"
          Left other  -> expectationFailure $
            "expected ConnectorInvalidCredential, got: " <> T.unpack (connectorErrorText other)
          Right _ -> expectationFailure "expected Left, got Right runner"

    it "returns ConnectorInvalidCredential when decryption fails" $
      withTestEnv $ \env -> do
        insertCorruptCredential env
        let cfg = missingCredConfig
              { connectorCredentialId = Just "cred-corrupt" }
        result <- runAppM env $ resolveConnectorRunner cfg
        case result of
          Left (ConnectorInvalidCredential _) -> pure ()
          Left other  -> expectationFailure $
            "expected ConnectorInvalidCredential, got: " <> T.unpack (connectorErrorText other)
          Right _ -> expectationFailure "expected Left, got Right runner"

    it "returns ConnectorUnsupported for unimplemented system types" $
      withTestEnv $ \env -> do
        insertLinearCredential env
        let cfg = unsupportedConfig
              { connectorCredentialId = Just "cred-linear" }
        result <- runAppM env $ resolveConnectorRunner cfg
        case result of
          Right runner -> do
            actions <- connectorAvailableActions runner
            actions `shouldBe` []
            execResult <- connectorExecuteAction runner (ActionName "anything") (toJSON ([] :: [Int]))
            case execResult of
              Left (ConnectorUnsupported _) -> pure ()
              Left other -> expectationFailure $
                "expected ConnectorUnsupported, got: " <> T.unpack (connectorErrorText other)
              Right _ -> expectationFailure "expected Left from executeAction, got Right"
          Left err -> expectationFailure $
            "expected Right runner for http system, got: " <> T.unpack (connectorErrorText err)

    it "returns a ConnectorRunner for a valid Linear credential with 3 actions" $
      withTestEnv $ \env -> do
        insertLinearCredential env
        result <- runAppM env $ resolveConnectorRunner linearConfig
        case result of
          Left err -> expectationFailure $
            "expected Right ConnectorRunner, got: " <> T.unpack (connectorErrorText err)
          Right runner -> do
            actions <- connectorAvailableActions runner
            length actions `shouldBe` 3
            let names = map actionSpecName actions
            names `shouldSatisfy` elem "list_issues"
            names `shouldSatisfy` elem "create_issue"
            names `shouldSatisfy` elem "update_issue"

    it "applies action filter to limit available actions" $
      withTestEnv $ \env -> do
        insertLinearCredential env
        let cfg = linearConfig { connectorActionFilter = ["list_issues"] }
        result <- runAppM env $ resolveConnectorRunner cfg
        case result of
          Left err -> expectationFailure $ "unexpected error: " <> show err
          Right runner -> do
            actions <- connectorAvailableActions runner
            -- Filter ["list_issues"] keeps only that one action
            length actions `shouldBe` 1
            map actionSpecName actions `shouldBe` ["list_issues"]
