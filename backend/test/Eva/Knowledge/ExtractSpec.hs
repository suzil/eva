{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Tests for EVA-77: Eva.Knowledge.Extract + Extract.Codebase.
--
-- All tests use an in-memory SQLite database and a fixture temp directory.
-- The LLM client is mocked so tests run without an API key.
module Eva.Knowledge.ExtractSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Data.IORef
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sqlite (createSqlitePool)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Eva.App (AppEnv (..), AppM, runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.Dispatch (execute)
import Eva.Engine.LLM
import Eva.Knowledge.Extract (extractForSource)
import Eva.Knowledge.Extract.Codebase (extractCodebase)
import Eva.Knowledge.Store (listBySource, updateEntry)
import Eva.Knowledge.Types
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries (insertProgram)

-- ---------------------------------------------------------------------------
-- Mock LLM client
-- ---------------------------------------------------------------------------

-- | Returns a canned response without calling any API.
mockLLMClient :: Text -> LLMClient
mockLLMClient response = LLMClient
  { clientCall   = \_ -> pure (Right (LLMResponse response Nothing (TokenUsage 10 5 15)))
  , clientStream = \_ _ -> pure (Right (LLMResponse response Nothing (TokenUsage 10 5 15)))
  }

-- | Counts how many times clientCall is invoked.
countingLLMClient :: IORef Int -> Text -> LLMClient
countingLLMClient ref response = LLMClient
  { clientCall   = \_ -> modifyIORef' ref (+1) >> pure (Right (LLMResponse response Nothing (TokenUsage 10 5 15)))
  , clientStream = \_ _ -> pure (Right (LLMResponse response Nothing (TokenUsage 10 5 15)))
  }

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

withTestEnv :: LLMClient -> (AppEnv -> IO ()) -> IO ()
withTestEnv llm action = do
  pool       <- runNoLoggingT $ createSqlitePool ":memory:" 1
  runMigrations pool
  broadcasts <- newTVarIO Map.empty
  cancelTokens <- newTVarIO Map.empty
  let cfg = AppConfig
        { configDbPath          = ":memory:"
        , configPort            = 8080
        , configLlmApiKey       = Nothing
        , configAnthropicApiKey = Nothing
        , configLogLevel        = LogError
        , configCredentialKey   = "test-key-for-extract-spec"
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
        , envCredentialKey   = Crypto.deriveKey "test-key-for-extract-spec"
        , envCancelTokens    = cancelTokens
        }
  action env

runTest :: AppEnv -> AppM a -> IO a
runTest = runAppM

-- ---------------------------------------------------------------------------
-- Fixture helpers
-- ---------------------------------------------------------------------------

-- | A minimal project fixture with Haskell + TypeScript files and a package.json.
setupFixtureDir :: FilePath -> IO ()
setupFixtureDir root = do
  createDirectory (root </> "backend")
  createDirectory (root </> "frontend")
  writeFile (root </> "backend" </> "Main.hs")
    "module Main where\nmain :: IO ()\nmain = putStrLn \"hello\"\n"
  writeFile (root </> "backend" </> "Lib.hs")
    "module Lib where\nfoo :: Int\nfoo = 42\n"
  writeFile (root </> "frontend" </> "App.tsx")
    "export default function App() { return <div>Eva</div>; }\n"
  writeFile (root </> "package.json")
    "{\"dependencies\":{\"react\":\"^18.0.0\"},\"devDependencies\":{\"typescript\":\"^5.0.0\"}}"
  writeFile (root </> "README.md") "# Test Project\n"
  writeFile (root </> "Makefile") "build:\n\tcabal build\n"

testProgramId :: ProgramId
testProgramId = ProgramId "prog-extract-test"

testProgram :: Program
testProgram = Program
  { programId          = testProgramId
  , programName        = "Extract Test"
  , programDescription = Nothing
  , programState       = Draft
  , programGraph     = Graph { graphNodes = Map.empty, graphEdges = [] }
  , programCreatedAt = read "2026-01-01 00:00:00 UTC"
  , programUpdatedAt = read "2026-01-01 00:00:00 UTC"
  }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "extractCodebase — 6 categories" $ do
    it "produces exactly 6 KnowledgeEntry rows for a fixture codebase" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        withTestEnv (mockLLMClient "This is a test codebase.") $ \env -> do
          runTest env (insertProgram testProgram)
          runTest env (extractCodebase root testProgramId)
          entries <- runTest env (listBySource SourceCodebase (Just (T.pack root)) testProgramId)
          length entries `shouldBe` 6

    it "produces entries for all 6 expected titles" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        withTestEnv (mockLLMClient "Summary text.") $ \env -> do
          runTest env (insertProgram testProgram)
          runTest env (extractCodebase root testProgramId)
          entries <- runTest env (listBySource SourceCodebase (Just (T.pack root)) testProgramId)
          let titles = map knowledgeEntryTitle entries
          titles `shouldContain` ["File Tree"]
          titles `shouldContain` ["Language Distribution"]
          titles `shouldContain` ["Key Files"]
          titles `shouldContain` ["Dependencies"]
          titles `shouldContain` ["Git Metadata"]
          titles `shouldContain` ["Codebase Structure Summary"]

    it "detects TypeScript and Haskell in language distribution" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        withTestEnv (mockLLMClient "Summary.") $ \env -> do
          runTest env (insertProgram testProgram)
          runTest env (extractCodebase root testProgramId)
          entries <- runTest env (listBySource SourceCodebase (Just (T.pack root)) testProgramId)
          let langEntry = head [e | e <- entries, knowledgeEntryTitle e == "Language Distribution"]
          knowledgeEntryContent langEntry `shouldSatisfy` T.isInfixOf "TypeScript"
          knowledgeEntryContent langEntry `shouldSatisfy` T.isInfixOf "Haskell"

    it "key files entry lists README.md and Makefile" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        withTestEnv (mockLLMClient "Summary.") $ \env -> do
          runTest env (insertProgram testProgram)
          runTest env (extractCodebase root testProgramId)
          entries <- runTest env (listBySource SourceCodebase (Just (T.pack root)) testProgramId)
          let kfEntry = head [e | e <- entries, knowledgeEntryTitle e == "Key Files"]
          knowledgeEntryContent kfEntry `shouldSatisfy` T.isInfixOf "README.md"
          knowledgeEntryContent kfEntry `shouldSatisfy` T.isInfixOf "Makefile"

    it "dependencies entry includes react from package.json" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        withTestEnv (mockLLMClient "Summary.") $ \env -> do
          runTest env (insertProgram testProgram)
          runTest env (extractCodebase root testProgramId)
          entries <- runTest env (listBySource SourceCodebase (Just (T.pack root)) testProgramId)
          let depsEntry = head [e | e <- entries, knowledgeEntryTitle e == "Dependencies"]
          knowledgeEntryContent depsEntry `shouldSatisfy` T.isInfixOf "react"

    it "structure summary uses the LLM response content" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        withTestEnv (mockLLMClient "This is a Haskell+TypeScript monorepo.") $ \env -> do
          runTest env (insertProgram testProgram)
          runTest env (extractCodebase root testProgramId)
          entries <- runTest env (listBySource SourceCodebase (Just (T.pack root)) testProgramId)
          let summaryEntry = head [e | e <- entries, knowledgeEntryTitle e == "Codebase Structure Summary"]
          knowledgeEntryContent summaryEntry `shouldBe` "This is a Haskell+TypeScript monorepo."

    it "summary confidence is 0.6; all other entries have confidence 1.0" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        withTestEnv (mockLLMClient "Summary.") $ \env -> do
          runTest env (insertProgram testProgram)
          runTest env (extractCodebase root testProgramId)
          entries <- runTest env (listBySource SourceCodebase (Just (T.pack root)) testProgramId)
          let summary  = [e | e <- entries, knowledgeEntryCategory e == CategorySummary]
              others   = [e | e <- entries, knowledgeEntryCategory e /= CategorySummary]
          map knowledgeEntryConfidence summary `shouldBe` [0.6]
          all ((== 1.0) . knowledgeEntryConfidence) others `shouldBe` True

  describe "extractCodebase — caching" $ do
    it "does NOT call the LLM a second time on re-extraction (summary is cached)" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        callCount <- newIORef (0 :: Int)
        withTestEnv (countingLLMClient callCount "Cached summary.") $ \env -> do
          runTest env (insertProgram testProgram)
          runTest env (extractCodebase root testProgramId)
          runTest env (extractCodebase root testProgramId)
          count <- readIORef callCount
          count `shouldBe` 1

    it "still produces exactly 6 entries after a second extraction" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        withTestEnv (mockLLMClient "Summary.") $ \env -> do
          runTest env (insertProgram testProgram)
          runTest env (extractCodebase root testProgramId)
          runTest env (extractCodebase root testProgramId)
          entries <- runTest env (listBySource SourceCodebase (Just (T.pack root)) testProgramId)
          length entries `shouldBe` 6

  describe "extractCodebase — is_edited preservation" $ do
    it "preserves is_edited=True entries through a refresh cycle" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        withTestEnv (mockLLMClient "Summary.") $ \env -> do
          runTest env (insertProgram testProgram)
          -- First extraction to get a summary entry
          runTest env (extractCodebase root testProgramId)
          entries1 <- runTest env (listBySource SourceCodebase (Just (T.pack root)) testProgramId)
          let summaryId = knowledgeEntryId $ head
                [e | e <- entries1, knowledgeEntryCategory e == CategorySummary]
          -- Mark the summary as user-edited
          let editedSummary = (head [e | e <- entries1, knowledgeEntryCategory e == CategorySummary])
                { knowledgeEntryContent  = "User-edited summary text."
                , knowledgeEntryIsEdited = True
                }
          runTest env (updateEntry editedSummary)
          -- Second extraction: edited summary should survive; LLM should not be called
          callCount <- newIORef (0 :: Int)
          let env2 = env { envLLMClient = countingLLMClient callCount "New summary." }
          runTest env2 (extractCodebase root testProgramId)
          entries2 <- runTest env2 (listBySource SourceCodebase (Just (T.pack root)) testProgramId)
          -- The edited summary should still be present with its custom content
          let survivingSummary = head [e | e <- entries2, knowledgeEntryId e == summaryId]
          knowledgeEntryContent survivingSummary `shouldBe` "User-edited summary text."
          knowledgeEntryIsEdited survivingSummary `shouldBe` True
          -- LLM was not called because the is_edited summary survived
          count <- readIORef callCount
          count `shouldBe` 0

    it "non-edited entries are deleted before re-extraction (no stale duplicates)" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        withTestEnv (mockLLMClient "Summary.") $ \env -> do
          runTest env (insertProgram testProgram)
          runTest env (extractCodebase root testProgramId)
          runTest env (extractCodebase root testProgramId)
          entries <- runTest env (listBySource SourceCodebase (Just (T.pack root)) testProgramId)
          -- Exactly 6 entries total after two refreshes — no stale duplicates
          length entries `shouldBe` 6

  describe "extractForSource — dispatch" $ do
    it "dispatches SourceCodebase to extractCodebase" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        withTestEnv (mockLLMClient "Summary.") $ \env -> do
          runTest env (insertProgram testProgram)
          runTest env (extractForSource SourceCodebase (Just (T.pack root)) testProgramId)
          entries <- runTest env (listBySource SourceCodebase (Just (T.pack root)) testProgramId)
          length entries `shouldBe` 6

    it "dispatches SourceLinear to extractLinear (fails on missing credential)" $
      withSystemTempDirectory "eva-extract-test" $ \root -> do
        setupFixtureDir root
        withTestEnv dummyLLMClient $ \env -> do
          runTest env (insertProgram testProgram)
          -- extractLinear is now wired; a missing credential produces an error
          runTest env (extractForSource SourceLinear (Just "nonexistent-cred") testProgramId)
            `shouldThrow` anyException

