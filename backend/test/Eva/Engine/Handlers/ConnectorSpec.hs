{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for EVA-30: Connector framework.
-- Tests resolveConnectorRunner in isolation: missing credential, invalid
-- credential (bad decrypt), unsupported system, and a valid Linear stub.
--
-- EVA-69: adds SystemCodebase connector tests — missing endpoint, codebase not
-- found in DB, read_file returns file content, write_file creates a changeset.
module Eva.Engine.Handlers.ConnectorSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Map.Strict as Map
import Data.Aeson (object, toJSON, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist.Sqlite (createSqlitePool)
import System.Directory (createFileLink)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Eva.App (AppEnv (..), runAppM)
import Eva.Codebase.Types
  ( CodebaseId (..)
  , CodeChangeset (..)
  , FileChange (..)
  )
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.Handlers.Connector (resolveConnectorRunner)
import Eva.Engine.LLM (dummyLLMClient)
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries
  ( insertCodebase
  , insertCredential
  , insertProgram
  , insertRun
  , insertStep
  , listChangesetsForRun
  )

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
-- Dummy RunId / StepId for tests that don't exercise write paths
-- ---------------------------------------------------------------------------

dummyRunId :: RunId
dummyRunId = RunId "test-run-id"

dummyStepId :: StepId
dummyStepId = StepId "test-step-id"

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

-- | A ConnectorConfig for the Codebase system with no endpoint.
codebaseNoEndpointConfig :: ConnectorConfig
codebaseNoEndpointConfig = ConnectorConfig
  { connectorSystem       = SystemCodebase
  , connectorCredentialId = Nothing
  , connectorEndpoint     = Nothing
  , connectorScope        = Nothing
  , connectorActionFilter = []
  }

-- | A ConnectorConfig for the Codebase system with a non-existent codebase ID.
codebaseMissingConfig :: ConnectorConfig
codebaseMissingConfig = ConnectorConfig
  { connectorSystem       = SystemCodebase
  , connectorCredentialId = Nothing
  , connectorEndpoint     = Just "cb-does-not-exist"
  , connectorScope        = Nothing
  , connectorActionFilter = []
  }

-- | A ConnectorConfig pointing at a real codebase in the DB.
codebaseConfig :: Text -> ConnectorConfig
codebaseConfig cbIdText = ConnectorConfig
  { connectorSystem       = SystemCodebase
  , connectorCredentialId = Nothing
  , connectorEndpoint     = Just cbIdText
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

-- | Insert a minimal program row so that codebase rows satisfy their FK.
insertTestProgram :: AppEnv -> ProgramId -> IO ()
insertTestProgram env pid = do
  now <- getCurrentTime
  let prog = Program
        { programId        = pid
        , programName      = "Connector test program"
        , programState     = Draft
        , programGraph     = Graph { graphNodes = mempty, graphEdges = [] }
        , programCreatedAt = now
        , programUpdatedAt = now
        }
  runAppM env $ insertProgram prog

-- | Insert a minimal run + step row so that changeset rows satisfy their FKs.
insertTestRunAndStep :: AppEnv -> RunId -> StepId -> ProgramId -> IO ()
insertTestRunAndStep env rid sid pid = do
  now <- getCurrentTime
  let run = Run
        { runId          = rid
        , runProgramId   = pid
        , runState       = RunRunning
        , runTriggerInfo = Nothing
        , runStartedAt   = Just now
        , runFinishedAt  = Nothing
        }
      step = Step
        { stepId          = sid
        , stepRunId       = rid
        , stepNodeId      = NodeId "test-node"
        , stepState       = StepRunning
        , stepInput       = Nothing
        , stepOutput      = Nothing
        , stepError       = Nothing
        , stepRetryCount  = 0
        , stepStartedAt   = Just now
        , stepFinishedAt  = Nothing
        }
  runAppM env $ do
    insertRun run
    insertStep step

-- | Insert a program + codebase row pointing at the given directory.
-- Uses a program ID derived from the codebase ID to avoid collisions.
insertTestCodebase :: AppEnv -> Text -> FilePath -> IO ()
insertTestCodebase env cbIdText dir = do
  now <- getCurrentTime
  let pid = ProgramId ("prog-for-" <> cbIdText)
  insertTestProgram env pid
  runAppM env $
    insertCodebase (CodebaseId cbIdText) pid (T.pack dir) now

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "resolveConnectorRunner — credential-based systems" $ do

    it "returns ConnectorMissingCredential when credentialId is Nothing" $
      withTestEnv $ \env -> do
        result <- runAppM env $ resolveConnectorRunner noCredConfig dummyRunId dummyStepId
        case result of
          Left (ConnectorMissingCredential _) -> pure ()
          Left other  -> expectationFailure $
            "expected ConnectorMissingCredential, got: " <> T.unpack (connectorErrorText other)
          Right _ -> expectationFailure "expected Left, got Right runner"

    it "returns ConnectorInvalidCredential when credential not found in DB" $
      withTestEnv $ \env -> do
        result <- runAppM env $ resolveConnectorRunner missingCredConfig dummyRunId dummyStepId
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
        result <- runAppM env $ resolveConnectorRunner cfg dummyRunId dummyStepId
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
        result <- runAppM env $ resolveConnectorRunner cfg dummyRunId dummyStepId
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
        result <- runAppM env $ resolveConnectorRunner linearConfig dummyRunId dummyStepId
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
        result <- runAppM env $ resolveConnectorRunner cfg dummyRunId dummyStepId
        case result of
          Left err -> expectationFailure $ "unexpected error: " <> show err
          Right runner -> do
            actions <- connectorAvailableActions runner
            length actions `shouldBe` 1
            map actionSpecName actions `shouldBe` ["list_issues"]

  describe "resolveConnectorRunner — SystemCodebase" $ do

    it "returns ConnectorMissingCredential when endpoint is Nothing" $
      withTestEnv $ \env -> do
        result <- runAppM env $
          resolveConnectorRunner codebaseNoEndpointConfig dummyRunId dummyStepId
        case result of
          Left (ConnectorMissingCredential _) -> pure ()
          Left other -> expectationFailure $
            "expected ConnectorMissingCredential, got: " <> T.unpack (connectorErrorText other)
          Right _ -> expectationFailure "expected Left, got Right runner"

    it "returns ConnectorApiError when codebase not found in DB" $
      withTestEnv $ \env -> do
        result <- runAppM env $
          resolveConnectorRunner codebaseMissingConfig dummyRunId dummyStepId
        case result of
          Left (ConnectorApiError msg) ->
            T.unpack msg `shouldContain` "cb-does-not-exist"
          Left other -> expectationFailure $
            "expected ConnectorApiError, got: " <> T.unpack (connectorErrorText other)
          Right _ -> expectationFailure "expected Left, got Right runner"

    it "exposes 4 actions for a connected codebase" $
      withTestEnv $ \env ->
        withSystemTempDirectory "eva-connector-test" $ \dir -> do
          insertTestCodebase env "cb-test-1" dir
          result <- runAppM env $
            resolveConnectorRunner (codebaseConfig "cb-test-1") dummyRunId dummyStepId
          case result of
            Left err -> expectationFailure $
              "expected Right runner, got: " <> T.unpack (connectorErrorText err)
            Right runner -> do
              actions <- connectorAvailableActions runner
              length actions `shouldBe` 4
              let names = map actionSpecName actions
              names `shouldSatisfy` elem "list_tree"
              names `shouldSatisfy` elem "read_file"
              names `shouldSatisfy` elem "git_diff"
              names `shouldSatisfy` elem "write_file"

    it "read_file action returns file content for a valid path" $
      withTestEnv $ \env ->
        withSystemTempDirectory "eva-connector-test" $ \dir -> do
          -- Write a test file into the temp directory
          let fileName = "hello.txt"
              fileContent = "hello from eva codebase connector\n"
          writeFile (dir <> "/" <> fileName) fileContent
          insertTestCodebase env "cb-test-2" dir
          result <- runAppM env $
            resolveConnectorRunner (codebaseConfig "cb-test-2") dummyRunId dummyStepId
          case result of
            Left err -> expectationFailure $
              "expected Right runner, got: " <> T.unpack (connectorErrorText err)
            Right runner -> do
              let args = object ["path" .= ("hello.txt" :: Text)]
              execResult <- connectorExecuteAction runner (ActionName "read_file") args
              case execResult of
                Left err2 -> expectationFailure $
                  "expected Right value, got: " <> T.unpack (connectorErrorText err2)
                Right val ->
                  case val of
                    _ -> do
                      -- Verify the response contains the file content
                      show val `shouldContain` "hello from eva codebase connector"

    it "read_file rejects a symlink that escapes the codebase root" $
      withTestEnv $ \env ->
        withSystemTempDirectory "eva-symlink-test" $ \dir -> do
          -- Place a symlink inside the codebase root pointing at /tmp (outside root).
          -- read_file should detect the escape and return an error.
          createFileLink "/tmp" (dir <> "/evil-link")
          insertTestCodebase env "cb-symlink-test" dir
          result <- runAppM env $
            resolveConnectorRunner (codebaseConfig "cb-symlink-test") dummyRunId dummyStepId
          case result of
            Left err -> expectationFailure $
              "expected Right runner, got: " <> T.unpack (connectorErrorText err)
            Right runner -> do
              let args = object ["path" .= ("evil-link/passwd" :: Text)]
              execResult <- connectorExecuteAction runner (ActionName "read_file") args
              case execResult of
                Right _ -> expectationFailure "expected Left (path escapes root), got Right"
                Left _  -> pure ()  -- any error is acceptable — the point is it doesn't succeed

    it "write_file action creates a pending CodeChangeset in the DB" $
      withTestEnv $ \env ->
        withSystemTempDirectory "eva-connector-test" $ \dir -> do
          insertTestCodebase env "cb-test-3" dir
          let testRunId  = RunId  "run-write-test"
              testStepId = StepId "step-write-test"
              testProgId = ProgramId "prog-for-write-test"
          -- Insert program/run/step rows to satisfy FK constraints on code_changesets
          insertTestProgram env testProgId
          insertTestRunAndStep env testRunId testStepId testProgId
          result <- runAppM env $
            resolveConnectorRunner (codebaseConfig "cb-test-3") testRunId testStepId
          case result of
            Left err -> expectationFailure $
              "expected Right runner, got: " <> T.unpack (connectorErrorText err)
            Right runner -> do
              let args = object
                    [ "path"    .= ("src/NewFile.hs" :: Text)
                    , "content" .= ("module NewFile where\n" :: Text)
                    , "action"  .= ("add" :: Text)
                    ]
              execResult <- connectorExecuteAction runner (ActionName "write_file") args
              case execResult of
                Left err2 -> expectationFailure $
                  "write_file failed: " <> T.unpack (connectorErrorText err2)
                Right _ -> do
                  -- Verify changeset was persisted in the DB
                  changesets <- runAppM env (listChangesetsForRun testRunId)
                  length changesets `shouldBe` 1
                  case changesets of
                    [] -> expectationFailure "expected 1 changeset, got 0"
                    (cs : _) -> do
                      show (codeChangesetStatus cs) `shouldContain` "ChangesetPending"
                      case codeChangesetFiles cs of
                        [] -> expectationFailure "expected 1 file change, got 0"
                        (fc : _) ->
                          T.unpack (fileChangePath fc) `shouldBe` "src/NewFile.hs"
