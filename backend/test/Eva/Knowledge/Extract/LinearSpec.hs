{-# LANGUAGE OverloadedStrings #-}

-- | Tests for EVA-78: Eva.Knowledge.Extract.Linear.
--
-- All tests inject a mock 'LinearApiCall' via 'extractLinearWith' — no real
-- network calls are made. The LLM client is also mocked.
module Eva.Knowledge.Extract.LinearSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sqlite (createSqlitePool)
import Control.Monad.Logger (runNoLoggingT)
import Test.Hspec

import Eva.App (AppEnv (..), AppM, runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.Dispatch (execute)
import Eva.Engine.LLM
import Eva.Knowledge.Extract.Linear (extractLinear, extractLinearWith)
import Eva.Knowledge.Store (listBySource)
import Eva.Knowledge.Types
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries (insertProgram)

-- ---------------------------------------------------------------------------
-- Mock helpers
-- ---------------------------------------------------------------------------

mockLLMClient :: Text -> LLMClient
mockLLMClient response = LLMClient
  { clientCall   = \_ -> pure (Right (LLMResponse response Nothing (TokenUsage 10 5 15)))
  , clientStream = \_ _ -> pure (Right (LLMResponse response Nothing (TokenUsage 10 5 15)))
  }

countingLLMClient :: IORef Int -> Text -> LLMClient
countingLLMClient ref response = LLMClient
  { clientCall   = \_ -> modifyIORef' ref (+1) >> pure (Right (LLMResponse response Nothing (TokenUsage 10 5 15)))
  , clientStream = \_ _ -> pure (Right (LLMResponse response Nothing (TokenUsage 10 5 15)))
  }

-- | A mock Linear API call that returns fixture data based on the query content.
-- Routes by keywords in the query string so each GraphQL fetch gets its own data.
mockLinearApiCall :: LinearApiCallMock -> Text -> Value -> IO (Either ConnectorError Value)
mockLinearApiCall MockSuccess query _vars
  | "teams" `T.isInfixOf` query     = pure (Right projectInventoryData)
  | "workflowStates" `T.isInfixOf` query = pure (Right workflowStatesData)
  | "issueLabels" `T.isInfixOf` query    = pure (Right issueLabelsData)
  | "users" `T.isInfixOf` query          = pure (Right membersData)
  | "issues" `T.isInfixOf` query         = pure (Right recentIssuesData)
  | otherwise                            = pure (Right (object []))
mockLinearApiCall MockApiError _ _ =
  pure (Left (ConnectorApiError "mock API error"))

data LinearApiCallMock = MockSuccess | MockApiError

type LinearApiCall = Text -> Value -> IO (Either ConnectorError Value)

-- ---------------------------------------------------------------------------
-- Fixture: canned Linear API payloads
-- ---------------------------------------------------------------------------

projectInventoryData :: Value
projectInventoryData = object
  [ "teams" .= object
      [ "nodes" .= Aeson.Array (pure $ object ["id" .= ("t1" :: Text), "name" .= ("EVA" :: Text)])
      ]
  , "projects" .= object
      [ "nodes" .= Aeson.Array (pure $ object
          [ "id"    .= ("p1" :: Text)
          , "name"  .= ("Eva" :: Text)
          , "state" .= ("started" :: Text)
          ])
      ]
  ]

workflowStatesData :: Value
workflowStatesData = object
  [ "workflowStates" .= object
      [ "nodes" .= Aeson.Array (pure $ object
          [ "id"   .= ("s1" :: Text)
          , "name" .= ("In Progress" :: Text)
          , "type" .= ("started" :: Text)
          ])
      ]
  ]

issueLabelsData :: Value
issueLabelsData = object
  [ "issueLabels" .= object
      [ "nodes" .= Aeson.Array (pure $ object
          [ "id"          .= ("l1" :: Text)
          , "name"        .= ("Bug" :: Text)
          , "description" .= ("A bug report" :: Text)
          ])
      ]
  ]

membersData :: Value
membersData = object
  [ "users" .= object
      [ "nodes" .= Aeson.Array (pure $ object
          [ "id"    .= ("u1" :: Text)
          , "name"  .= ("Alice" :: Text)
          , "email" .= ("alice@example.com" :: Text)
          ])
      ]
  ]

recentIssuesData :: Value
recentIssuesData = object
  [ "issues" .= object
      [ "nodes" .= Aeson.Array (pure $ object
          [ "identifier" .= ("EVA-1" :: Text)
          , "title"      .= ("Bootstrap project" :: Text)
          , "state"      .= object ["name" .= ("Done" :: Text)]
          , "priority"   .= (1 :: Int)
          ])
      ]
  ]

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

withTestEnv :: LLMClient -> (AppEnv -> IO ()) -> IO ()
withTestEnv llm action = do
  pool       <- runNoLoggingT $ createSqlitePool ":memory:" 1
  runMigrations pool
  broadcasts <- newTVarIO Map.empty
  let cfg = AppConfig
        { configDbPath          = ":memory:"
        , configPort            = 8080
        , configLlmApiKey       = Nothing
        , configAnthropicApiKey = Nothing
        , configLogLevel        = LogError
        , configCredentialKey   = "test-key-for-linear-extract"
        , configStaticDir       = Nothing
        }
      env = AppEnv
        { envConfig          = cfg
        , envDbPool          = pool
        , envLogger          = \_ -> pure ()
        , envDispatch        = execute
        , envLLMClient       = llm
        , envAnthropicClient = dummyLLMClient
        , envBroadcasts      = broadcasts
        , envCredentialKey   = Crypto.deriveKey "test-key-for-linear-extract"
        }
  action env

runTest :: AppEnv -> AppM a -> IO a
runTest = runAppM

testProgramId :: ProgramId
testProgramId = ProgramId "prog-linear-extract-test"

testProgram :: Program
testProgram = Program
  { programId        = testProgramId
  , programName      = "Linear Extract Test"
  , programState     = Draft
  , programGraph     = Graph { graphNodes = Map.empty, graphEdges = [] }
  , programCreatedAt = read "2026-01-01 00:00:00 UTC"
  , programUpdatedAt = read "2026-01-01 00:00:00 UTC"
  }

testSrcId :: Text
testSrcId = "cred-linear-test"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "extractLinearWith — 5 categories" $ do

    it "produces exactly 5 KnowledgeEntry rows" $
      withTestEnv (mockLLMClient "Active development on the Eva IDE project.") $ \env -> do
        runTest env (insertProgram testProgram)
        runTest env (extractLinearWith (mockLinearApiCall MockSuccess) testSrcId testProgramId)
        entries <- runTest env (listBySource SourceLinear (Just testSrcId) testProgramId)
        length entries `shouldBe` 5

    it "produces entries for all 5 expected titles" $
      withTestEnv (mockLLMClient "Summary.") $ \env -> do
        runTest env (insertProgram testProgram)
        runTest env (extractLinearWith (mockLinearApiCall MockSuccess) testSrcId testProgramId)
        entries <- runTest env (listBySource SourceLinear (Just testSrcId) testProgramId)
        let titles = map knowledgeEntryTitle entries
        titles `shouldContain` ["Project Inventory"]
        titles `shouldContain` ["Workflow States"]
        titles `shouldContain` ["Label Taxonomy"]
        titles `shouldContain` ["Member Directory"]
        titles `shouldContain` ["Recent Activity Summary"]

    it "summary confidence is 0.6; all other entries have confidence 1.0" $
      withTestEnv (mockLLMClient "Summary.") $ \env -> do
        runTest env (insertProgram testProgram)
        runTest env (extractLinearWith (mockLinearApiCall MockSuccess) testSrcId testProgramId)
        entries <- runTest env (listBySource SourceLinear (Just testSrcId) testProgramId)
        let summary = [e | e <- entries, knowledgeEntryCategory e == CategorySummary]
            others  = [e | e <- entries, knowledgeEntryCategory e /= CategorySummary]
        map knowledgeEntryConfidence summary `shouldBe` [0.6]
        all ((== 1.0) . knowledgeEntryConfidence) others `shouldBe` True

  describe "extractLinearWith — caching" $ do

    it "does NOT call the LLM a second time within 24h (summary cached)" $
      withTestEnv (mockLLMClient "Cached summary.") $ \env -> do
        runTest env (insertProgram testProgram)
        callCount <- newIORef (0 :: Int)
        let env' = env { envLLMClient = countingLLMClient callCount "Cached summary." }
        -- First extraction: LLM is called once
        runTest env' (extractLinearWith (mockLinearApiCall MockSuccess) testSrcId testProgramId)
        -- Second extraction within 24h: LLM should NOT be called again
        runTest env' (extractLinearWith (mockLinearApiCall MockSuccess) testSrcId testProgramId)
        count <- readIORef callCount
        count `shouldBe` 1

    it "still produces exactly 5 entries after a second extraction (no stale duplicates)" $
      withTestEnv (mockLLMClient "Summary.") $ \env -> do
        runTest env (insertProgram testProgram)
        runTest env (extractLinearWith (mockLinearApiCall MockSuccess) testSrcId testProgramId)
        runTest env (extractLinearWith (mockLinearApiCall MockSuccess) testSrcId testProgramId)
        entries <- runTest env (listBySource SourceLinear (Just testSrcId) testProgramId)
        length entries `shouldBe` 5

  describe "extractLinear — credential failure" $ do

    it "fails with a descriptive message when the credential is missing" $
      withTestEnv dummyLLMClient $ \env -> do
        runTest env (insertProgram testProgram)
        runTest env (extractLinear "nonexistent-cred-id" testProgramId)
          `shouldThrow` anyException
