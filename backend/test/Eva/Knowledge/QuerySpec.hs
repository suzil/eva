{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for EVA-79: Eva.Knowledge.Query.
-- All tests run against an in-memory SQLite database with real FTS5 indexes.
module Eva.Knowledge.QuerySpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (forM_)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (object, (.=))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import Eva.App (AppEnv (..), AppM, runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Core.Types
import Eva.Engine.Dispatch (execute)
import Eva.Engine.LLM (dummyLLMClient)
import Eva.Knowledge.Query
import Eva.Knowledge.Store (insertEntry)
import Eva.Knowledge.Types
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Queries (insertProgram)

-- ---------------------------------------------------------------------------
-- Test environment (same pattern as StoreSpec)
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

testPid :: ProgramId
testPid = "query-prog-1"

testProg :: Program
testProg = Program
  { programId        = testPid
  , programName      = "Query Test Program"
  , programState     = Draft
  , programGraph     = Graph { graphNodes = Map.empty, graphEdges = [] }
  , programCreatedAt = t0
  , programUpdatedAt = t0
  }

-- | Minimal entry template — override fields as needed in each test.
baseEntry :: KnowledgeEntryId -> Text -> Text -> KnowledgeEntry
baseEntry eid title content = KnowledgeEntry
  { knowledgeEntryId              = eid
  , knowledgeEntrySourceType      = SourceCodebase
  , knowledgeEntrySourceId        = Just "src/"
  , knowledgeEntryProgramId       = Just testPid
  , knowledgeEntryCategory        = CategoryStructure
  , knowledgeEntryTitle           = title
  , knowledgeEntryContent         = content
  , knowledgeEntryOriginalContent = Just content
  , knowledgeEntryMetadata        = object []
  , knowledgeEntryConfidence      = 1.0
  , knowledgeEntryIsEdited        = False
  , knowledgeEntryCreatedAt       = t0
  , knowledgeEntryUpdatedAt       = t0
  , knowledgeEntryScannedAt       = t0
  }

emptyQuery :: SearchQuery
emptyQuery = SearchQuery
  { searchQueryText       = ""
  , searchQuerySourceType = Nothing
  , searchQueryCategory   = Nothing
  , searchQueryProgramId  = Nothing
  , searchQueryLimit      = Nothing
  }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Eva.Knowledge.Query" $ do

  -- -------------------------------------------------------------------------
  -- Keyword search (FTS5 BM25)
  -- -------------------------------------------------------------------------

  describe "search (keyword)" $ do

    it "returns BM25-ranked results — most relevant entry comes first" $ do
      withTestEnv $ \env -> do
        runTest env $ insertProgram testProg
        -- Entry A: "Haskell" appears once in content
        let entryA = baseEntry "ke-kw-a" "Intro" "Haskell is a language."
        -- Entry B: "Haskell" appears four times — should rank higher
        let entryB = (baseEntry "ke-kw-b" "Deep Haskell"
                       "Haskell functional Haskell types Haskell monad Haskell")
                       { knowledgeEntryConfidence = 0.5 }
        runTest env $ do
          insertEntry entryA
          insertEntry entryB
        results <- runTest env $ search emptyQuery
          { searchQueryText  = "Haskell"
          , searchQueryProgramId = Just testPid
          }
        length results `shouldBe` 2
        case results of
          (r : _) -> knowledgeEntryId (searchResultEntry r) `shouldBe` "ke-kw-b"
          []      -> expectationFailure "expected at least one result"

    it "returns an empty list when no entries match the query" $ do
      withTestEnv $ \env -> do
        runTest env $ do
          insertProgram testProg
          insertEntry (baseEntry "ke-kw-c" "File Tree" "directory structure listing")
        results <- runTest env $ search emptyQuery
          { searchQueryText = "xyzzy_nonexistent_term" }
        results `shouldBe` []

  -- -------------------------------------------------------------------------
  -- Structured search (SQL WHERE)
  -- -------------------------------------------------------------------------

  describe "search (structured)" $ do

    it "filters by category" $ do
      withTestEnv $ \env -> do
        runTest env $ insertProgram testProg
        let structEntry = (baseEntry "ke-st-a" "Git Metadata" "branch: main")
              { knowledgeEntryCategory = CategoryMetadata }
            otherEntry  = baseEntry "ke-st-b" "File Tree" "root directory"
        runTest env $ do
          insertEntry structEntry
          insertEntry otherEntry
        results <- runTest env $ search emptyQuery
          { searchQueryCategory  = Just CategoryMetadata
          , searchQueryProgramId = Just testPid
          }
        map (knowledgeEntryId . searchResultEntry) results `shouldBe` ["ke-st-a"]

    it "filters by source_type" $ do
      withTestEnv $ \env -> do
        runTest env $ insertProgram testProg
        let linearEntry = (baseEntry "ke-st-c" "Linear Project" "sprint goals")
              { knowledgeEntrySourceType = SourceLinear }
            cbEntry     = baseEntry "ke-st-d" "Codebase Files" "src structure"
        runTest env $ do
          insertEntry linearEntry
          insertEntry cbEntry
        results <- runTest env $ search emptyQuery
          { searchQuerySourceType = Just SourceLinear
          , searchQueryProgramId  = Just testPid
          }
        map (knowledgeEntryId . searchResultEntry) results `shouldBe` ["ke-st-c"]

  -- -------------------------------------------------------------------------
  -- Combined search (FTS5 + struct WHERE)
  -- -------------------------------------------------------------------------

  describe "search (combined)" $ do

    it "returns only entries matching both the text query and struct filter" $ do
      withTestEnv $ \env -> do
        runTest env $ insertProgram testProg
        let -- Matches text but wrong source_type
            e1 = (baseEntry "ke-cb-a" "Linear issues" "sprint backlog items")
              { knowledgeEntrySourceType = SourceLinear }
            -- Matches text AND source_type
            e2 = (baseEntry "ke-cb-b" "Codebase sprint" "sprint planning files")
              { knowledgeEntrySourceType = SourceCodebase }
            -- Wrong text, right source_type
            e3 = (baseEntry "ke-cb-c" "Language stats" "TypeScript 42 files")
              { knowledgeEntrySourceType = SourceCodebase }
        runTest env $ do
          insertEntry e1
          insertEntry e2
          insertEntry e3
        results <- runTest env $ search emptyQuery
          { searchQueryText       = "sprint"
          , searchQuerySourceType = Just SourceCodebase
          , searchQueryProgramId  = Just testPid
          }
        map (knowledgeEntryId . searchResultEntry) results `shouldBe` ["ke-cb-b"]

  -- -------------------------------------------------------------------------
  -- Path search
  -- -------------------------------------------------------------------------

  describe "searchByPath" $ do

    it "finds entries whose metadata path matches the LIKE pattern" $ do
      withTestEnv $ \env -> do
        runTest env $ insertProgram testProg
        let e1 = (baseEntry "ke-p-a" "Agent Handler" "handles agent nodes")
              { knowledgeEntryMetadata =
                  object ["path" .= ("backend/src/Eva/Engine/Handlers/Agent.hs" :: Text)] }
            e2 = (baseEntry "ke-p-b" "LLM Client" "openai client implementation")
              { knowledgeEntryMetadata =
                  object ["path" .= ("backend/src/Eva/Engine/LLM.hs" :: Text)] }
            e3 = (baseEntry "ke-p-c" "Frontend Types" "typescript types")
              { knowledgeEntryMetadata =
                  object ["path" .= ("frontend/src/types/index.ts" :: Text)] }
        runTest env $ do
          insertEntry e1
          insertEntry e2
          insertEntry e3
        -- Search for all .hs files
        results <- runTest env $ searchByPath "%.hs" Nothing
        let ids = map (knowledgeEntryId . searchResultEntry) results
        ids `shouldContain` ["ke-p-a"]
        ids `shouldContain` ["ke-p-b"]
        ids `shouldNotContain` ["ke-p-c"]

    it "scopes results by program_id when provided" $ do
      withTestEnv $ \env -> do
        let otherPid  = ProgramId "other-prog"
            otherProg = testProg { programId = otherPid, programName = "Other Program" }
        runTest env $ do
          insertProgram testProg
          insertProgram otherProg
        let e1 = (baseEntry "ke-p-d" "Module A" "foo")
              { knowledgeEntryProgramId = Just testPid
              , knowledgeEntryMetadata  = object ["path" .= ("src/A.hs" :: Text)] }
            e2 = (baseEntry "ke-p-e" "Module B" "bar")
              { knowledgeEntryProgramId = Just otherPid
              , knowledgeEntryMetadata  = object ["path" .= ("src/B.hs" :: Text)] }
        runTest env $ do
          insertEntry e1
          insertEntry e2
        results <- runTest env $ searchByPath "%.hs" (Just testPid)
        map (knowledgeEntryId . searchResultEntry) results `shouldBe` ["ke-p-d"]

  -- -------------------------------------------------------------------------
  -- assembleAgentContext
  -- -------------------------------------------------------------------------

  describe "assembleAgentContext" $ do

    it "returns non-empty text for a program with extracted entries" $ do
      withTestEnv $ \env -> do
        runTest env $ do
          insertProgram testProg
          insertEntry (baseEntry "ke-ac-a" "File Tree" "root: backend, frontend")
          insertEntry (baseEntry "ke-ac-b" "Language Distribution" "Haskell: 52, TypeScript: 41")
          insertEntry (baseEntry "ke-ac-c" "Git Metadata" "branch: main, status: clean")
        ctx <- runTest env $ assembleAgentContext testPid Nothing
        ctx `shouldNotBe` ""
        T.unpack ctx `shouldContain` "**"

    it "returns empty text when no entries exist for the program" $ do
      withTestEnv $ \env -> do
        runTest env $ insertProgram testProg
        ctx <- runTest env $ assembleAgentContext testPid Nothing
        ctx `shouldBe` ""

    it "includes at most 3 entries, prioritising by confidence" $ do
      withTestEnv $ \env -> do
        let mkEntry eid lbl conf =
              (baseEntry eid lbl "content") { knowledgeEntryConfidence = conf }
        runTest env $ do
          insertProgram testProg
          insertEntry (mkEntry "ke-ac-d" "Low Conf"    0.30)
          insertEntry (mkEntry "ke-ac-e" "Mid Conf"    0.60)
          insertEntry (mkEntry "ke-ac-f" "High Conf"   0.90)
          insertEntry (mkEntry "ke-ac-g" "Higher Conf" 0.95)
          insertEntry (mkEntry "ke-ac-h" "Highest"     1.00)
        ctx <- runTest env $ assembleAgentContext testPid Nothing
        -- At most 3 dividers means at most 3 entries
        let sections = T.splitOn "---" ctx
        length sections `shouldSatisfy` (<= 3)
        T.unpack ctx `shouldContain` "Highest"
        T.unpack ctx `shouldContain` "Higher Conf"
        T.unpack ctx `shouldNotContain` "Low Conf"

    it "truncates context to at most 2000 characters" $ do
      withTestEnv $ \env -> do
        runTest env $ do
          insertProgram testProg
          -- Insert 3 entries with very long content
          let longText = T.replicate 800 "x"
          insertEntry (baseEntry "ke-ac-i" "Entry 1" longText)
          insertEntry (baseEntry "ke-ac-j" "Entry 2" longText)
          insertEntry (baseEntry "ke-ac-k" "Entry 3" longText)
        ctx <- runTest env $ assembleAgentContext testPid Nothing
        T.length ctx `shouldSatisfy` (<= 2000)

  -- -------------------------------------------------------------------------
  -- FTS5 special-character escaping
  -- -------------------------------------------------------------------------

  describe "FTS escaping" $ do

    it "handles a double-quote in the search query without SQL error" $ do
      withTestEnv $ \env -> do
        runTest env $ do
          insertProgram testProg
          insertEntry (baseEntry "ke-esc-a" "Quoted" "content with \"quotes\"")
        -- If escapeFts is broken this will throw; we just check it doesn't error
        results <- runTest env $ search emptyQuery { searchQueryText = "\"malformed" }
        -- Any result (including empty) is acceptable — no exception is the test
        results `shouldSatisfy` (const True)

    it "handles an asterisk in the search query without SQL error" $ do
      withTestEnv $ \env -> do
        runTest env $ do
          insertProgram testProg
          insertEntry (baseEntry "ke-esc-b" "Star" "content with star operator")
        results <- runTest env $ search emptyQuery { searchQueryText = "star*" }
        results `shouldSatisfy` (const True)

  -- -------------------------------------------------------------------------
  -- Latency benchmarks (EVA-106: all 4 search modes must be <10ms at 1000
  -- entries; assembleAgentContext must be <50ms at 1000 entries)
  -- -------------------------------------------------------------------------

  describe "search latency" $ do

    it "keyword (FTS5) search over 1000 entries completes in under 10ms" $ do
      withTestEnv $ \env -> do
        runTest env $ insertProgram testProg
        forM_ [1 .. 1000 :: Int] $ \i ->
          runTest env $ insertEntry (baseEntry
            (KnowledgeEntryId ("ke-lat-kw-" <> T.pack (show i)))
            ("Title " <> T.pack (show i))
            ("Content about performance benchmarking entry number " <> T.pack (show i)))
        t0' <- getCurrentTime
        _ <- runTest env $ search emptyQuery { searchQueryText = "performance" }
        t1' <- getCurrentTime
        let diffMs = realToFrac (diffUTCTime t1' t0') * 1000 :: Double
        diffMs `shouldSatisfy` (< 10.0)

    it "structured (SQL WHERE) search over 1000 entries completes in under 10ms" $ do
      withTestEnv $ \env -> do
        runTest env $ insertProgram testProg
        forM_ [1 .. 1000 :: Int] $ \i ->
          runTest env $ insertEntry (baseEntry
            (KnowledgeEntryId ("ke-lat-st-" <> T.pack (show i)))
            ("Title " <> T.pack (show i))
            ("Structured content entry number " <> T.pack (show i)))
        t0' <- getCurrentTime
        _ <- runTest env $ search emptyQuery
              { searchQueryProgramId = Just testPid
              , searchQueryCategory  = Just CategoryStructure
              }
        t1' <- getCurrentTime
        let diffMs = realToFrac (diffUTCTime t1' t0') * 1000 :: Double
        diffMs `shouldSatisfy` (< 10.0)

    it "combined (FTS5 + WHERE) search over 1000 entries completes in under 10ms" $ do
      withTestEnv $ \env -> do
        runTest env $ insertProgram testProg
        forM_ [1 .. 1000 :: Int] $ \i ->
          runTest env $ insertEntry (baseEntry
            (KnowledgeEntryId ("ke-lat-co-" <> T.pack (show i)))
            ("Title " <> T.pack (show i))
            ("Combined benchmarking content entry " <> T.pack (show i)))
        t0' <- getCurrentTime
        _ <- runTest env $ search emptyQuery
              { searchQueryText      = "benchmarking"
              , searchQueryProgramId = Just testPid
              }
        t1' <- getCurrentTime
        let diffMs = realToFrac (diffUTCTime t1' t0') * 1000 :: Double
        diffMs `shouldSatisfy` (< 10.0)

    it "path search over 1000 entries completes in under 10ms" $ do
      withTestEnv $ \env -> do
        runTest env $ insertProgram testProg
        forM_ [1 .. 1000 :: Int] $ \i ->
          runTest env $ insertEntry
            (baseEntry
              (KnowledgeEntryId ("ke-lat-pa-" <> T.pack (show i)))
              ("Title " <> T.pack (show i))
              ("Path entry content " <> T.pack (show i)))
            { knowledgeEntryMetadata = object [ "path" .= ("/src/Eva/Module" <> T.pack (show i) <> ".hs" :: Text) ]
            }
        t0' <- getCurrentTime
        _ <- runTest env $ searchByPath "%.hs" (Just testPid)
        t1' <- getCurrentTime
        let diffMs = realToFrac (diffUTCTime t1' t0') * 1000 :: Double
        diffMs `shouldSatisfy` (< 10.0)

    it "assembleAgentContext over 1000 entries completes in under 50ms" $ do
      withTestEnv $ \env -> do
        let otherPid = "query-prog-2" :: ProgramId
            otherProg = testProg
              { programId   = otherPid
              , programName = "Other Program"
              }
        runTest env $ insertProgram testProg
        runTest env $ insertProgram otherProg
        -- Insert 500 entries for testPid, 500 for another program
        forM_ [1 .. 500 :: Int] $ \i ->
          runTest env $ insertEntry
            (baseEntry
              (KnowledgeEntryId ("ke-lat-ac-a-" <> T.pack (show i)))
              ("Context Title " <> T.pack (show i))
              ("Context content entry " <> T.pack (show i)))
            { knowledgeEntryConfidence = fromIntegral i / 500.0 }
        forM_ [1 .. 500 :: Int] $ \i ->
          runTest env $ insertEntry
            (baseEntry
              (KnowledgeEntryId ("ke-lat-ac-b-" <> T.pack (show i)))
              ("Other Title " <> T.pack (show i))
              ("Other content entry " <> T.pack (show i)))
            { knowledgeEntryProgramId = Just otherPid }
        t0' <- getCurrentTime
        _ <- runTest env $ assembleAgentContext testPid Nothing
        t1' <- getCurrentTime
        let diffMs = realToFrac (diffUTCTime t1' t0') * 1000 :: Double
        diffMs `shouldSatisfy` (< 50.0)
