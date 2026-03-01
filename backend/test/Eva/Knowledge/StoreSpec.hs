{-# LANGUAGE OverloadedStrings #-}

module Eva.Knowledge.StoreSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import Eva.App (AppEnv (..), AppM, runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.Dispatch (execute)
import Eva.Engine.LLM (dummyLLMClient)
import Eva.Knowledge.Store
import Eva.Knowledge.Types
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries (insertProgram)

-- ---------------------------------------------------------------------------
-- Test environment (same pattern as PersistenceSpec)
-- ---------------------------------------------------------------------------

withTestEnv :: (AppEnv -> IO ()) -> IO ()
withTestEnv action = do
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
        , configCredentialKey   = "test-key"
        , configStaticDir       = Nothing
        }
      env = AppEnv
        { envConfig          = cfg
        , envDbPool          = pool
        , envLogger          = \_ -> pure ()
        , envDispatch        = execute
        , envLLMClient       = dummyLLMClient
        , envAnthropicClient = dummyLLMClient
        , envBroadcasts      = broadcasts
        , envCredentialKey   = Crypto.deriveKey "test-key"
        , envCancelTokens    = cancelTokens
        }
  action env

runTest :: AppEnv -> AppM a -> IO a
runTest = runAppM

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

t0 :: UTCTime
t0 = posixSecondsToUTCTime 1_740_000_000

t1 :: UTCTime
t1 = posixSecondsToUTCTime 1_740_001_000

sampleEntry :: KnowledgeEntry
sampleEntry = KnowledgeEntry
  { knowledgeEntryId              = "ke-1"
  , knowledgeEntrySourceType      = SourceCodebase
  , knowledgeEntrySourceId        = Just "src/Foo.hs"
  , knowledgeEntryProgramId       = Nothing
  , knowledgeEntryCategory        = CategoryStructure
  , knowledgeEntryTitle           = "Module Foo"
  , knowledgeEntryContent         = "Defines the Foo module."
  , knowledgeEntryOriginalContent = Just "Defines the Foo module."
  , knowledgeEntryMetadata        = Null
  , knowledgeEntryConfidence      = 0.9
  , knowledgeEntryIsEdited        = False
  , knowledgeEntryCreatedAt       = t0
  , knowledgeEntryUpdatedAt       = t0
  , knowledgeEntryScannedAt       = t0
  }

-- | A second entry scoped to a specific program (for listEntries filtering).
-- Requires progA to be inserted first (FK constraint on program_id).
sampleEntry2 :: KnowledgeEntry
sampleEntry2 = sampleEntry
  { knowledgeEntryId        = "ke-2"
  , knowledgeEntryProgramId = Just "prog-a"
  , knowledgeEntryTitle     = "Module Bar"
  }

progA :: Program
progA = Program
  { programId          = "prog-a"
  , programName        = "Program A"
  , programDescription = Nothing
  , programState       = Draft
  , programGraph     = Graph { graphNodes = Map.empty, graphEdges = [] }
  , programCreatedAt = t0
  , programUpdatedAt = t0
  }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = around withTestEnv $ do

  describe "insertEntry / getEntry" $ do
    it "round-trips a KnowledgeEntry through the database" $ \env -> do
      runTest env (insertEntry sampleEntry)
      result <- runTest env (getEntry "ke-1")
      result `shouldBe` Just sampleEntry

    it "getEntry returns Nothing for an unknown id" $ \env -> do
      result <- runTest env (getEntry "no-such-entry")
      result `shouldBe` Nothing

  describe "updateEntry" $ do
    it "persists field changes and getEntry reflects them" $ \env -> do
      runTest env (insertEntry sampleEntry)
      let updated = sampleEntry
            { knowledgeEntryTitle     = "Module Foo (edited)"
            , knowledgeEntryContent   = "Updated content."
            , knowledgeEntryIsEdited  = True
            , knowledgeEntryUpdatedAt = t1
            }
      runTest env (updateEntry updated)
      result <- runTest env (getEntry "ke-1")
      fmap knowledgeEntryTitle    result `shouldBe` Just "Module Foo (edited)"
      fmap knowledgeEntryContent  result `shouldBe` Just "Updated content."
      fmap knowledgeEntryIsEdited result `shouldBe` Just True

  describe "deleteEntry" $ do
    it "removes the entry so getEntry returns Nothing" $ \env -> do
      runTest env (insertEntry sampleEntry)
      runTest env (deleteEntry "ke-1")
      result <- runTest env (getEntry "ke-1")
      result `shouldBe` Nothing

  describe "listEntries" $ do
    it "returns only entries matching the given programId" $ \env -> do
      runTest env (insertProgram progA)
      runTest env (insertEntry sampleEntry)   -- programId = Nothing
      runTest env (insertEntry sampleEntry2)  -- programId = Just "prog-a"
      entries <- runTest env (listEntries "prog-a")
      map knowledgeEntryId entries `shouldBe` ["ke-2"]

  describe "resetToAutoGenerated" $ do
    it "restores original content and clears is_edited" $ \env -> do
      let edited = sampleEntry
            { knowledgeEntryContent  = "Hand-edited content."
            , knowledgeEntryIsEdited = True
            , knowledgeEntryUpdatedAt = t1
            }
      runTest env (insertEntry edited)
      runTest env (resetToAutoGenerated "ke-1")
      result <- runTest env (getEntry "ke-1")
      fmap knowledgeEntryContent  result `shouldBe` Just "Defines the Foo module."
      fmap knowledgeEntryIsEdited result `shouldBe` Just False

    it "is a no-op when entry has no originalContent" $ \env -> do
      let noOriginal = sampleEntry
            { knowledgeEntryOriginalContent = Nothing
            , knowledgeEntryContent         = "Some content."
            , knowledgeEntryIsEdited        = True
            }
      runTest env (insertEntry noOriginal)
      runTest env (resetToAutoGenerated "ke-1")
      result <- runTest env (getEntry "ke-1")
      -- unchanged: content and isEdited should be what was inserted
      fmap knowledgeEntryContent  result `shouldBe` Just "Some content."
      fmap knowledgeEntryIsEdited result `shouldBe` Just True

    it "is a no-op when the entry does not exist" $ \env -> do
      -- should not throw
      runTest env (resetToAutoGenerated "nonexistent") `shouldReturn` ()
