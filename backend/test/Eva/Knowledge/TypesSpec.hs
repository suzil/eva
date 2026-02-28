{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eva.Knowledge.TypesSpec (spec) where

import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sql
  ( ConnectionPool
  , PersistValue (..)
  , Single (..)
  , delete
  , insertKey
  , rawSql
  , runSqlPool
  , update
  , (=.)
  )
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec
import Test.QuickCheck

import Eva.Core.TypesSpec ()
import Eva.Knowledge.Types
import Eva.Persistence.Migration (runMigrations)
import Eva.Persistence.Schema

-- ---------------------------------------------------------------------------
-- Test pool
-- ---------------------------------------------------------------------------

withTestPool :: (ConnectionPool -> IO ()) -> IO ()
withTestPool action = do
  pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
  runMigrations pool
  action pool

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

arbitraryText :: Gen Text
arbitraryText = T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ ['-', '_']))

arbitraryUTCTime :: Gen UTCTime
arbitraryUTCTime = do
  secs <- choose (0 :: Int, 1_735_689_600)
  pure $ posixSecondsToUTCTime (fromIntegral secs)

roundtrip :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Property
roundtrip x = eitherDecode (encode x) === Right x

t0 :: UTCTime
t0 = posixSecondsToUTCTime 1_740_000_000

-- ---------------------------------------------------------------------------
-- Arbitrary instances — identifiers
-- ---------------------------------------------------------------------------

instance Arbitrary KnowledgeEntryId where
  arbitrary = KnowledgeEntryId <$> arbitraryText

-- ---------------------------------------------------------------------------
-- Arbitrary instances — enums
-- ---------------------------------------------------------------------------

instance Arbitrary KnowledgeSourceType where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary KnowledgeCategory where
  arbitrary = elements [minBound .. maxBound]

-- ---------------------------------------------------------------------------
-- Arbitrary instances — records
-- ---------------------------------------------------------------------------

instance Arbitrary KnowledgeEntry where
  arbitrary =
    KnowledgeEntry
      <$> arbitrary
      <*> arbitrary
      <*> oneof [pure Nothing, Just <$> arbitraryText]
      <*> pure Nothing
      <*> arbitrary
      <*> arbitraryText
      <*> arbitraryText
      <*> oneof [pure Nothing, Just <$> arbitraryText]
      <*> pure (object [])
      <*> choose (0.0, 1.0)
      <*> arbitrary
      <*> arbitraryUTCTime
      <*> arbitraryUTCTime
      <*> arbitraryUTCTime

instance Arbitrary SearchQuery where
  arbitrary =
    SearchQuery
      <$> arbitraryText
      <*> oneof [pure Nothing, Just <$> arbitrary]
      <*> oneof [pure Nothing, Just <$> arbitrary]
      <*> pure Nothing
      <*> oneof [pure Nothing, Just <$> choose (1, 100)]

instance Arbitrary SearchResult where
  arbitrary =
    SearchResult
      <$> arbitrary
      <*> choose (0.0, 1.0)

-- ---------------------------------------------------------------------------
-- Sample row for FTS tests
-- ---------------------------------------------------------------------------

sampleKey :: KnowledgeEntryRowId
sampleKey = KnowledgeEntryRowKey "ke-fts-test-1"

sampleRow :: KnowledgeEntryRow
sampleRow =
  KnowledgeEntryRow
    { knowledgeEntryRowSourceType = "codebase"
    , knowledgeEntryRowSourceId = Just "src/Eva/Engine/Runner.hs"
    , knowledgeEntryRowProgramId = Nothing
    , knowledgeEntryRowCategory = "structure"
    , knowledgeEntryRowTitle = "Eva.Engine.Runner module"
    , knowledgeEntryRowContent = "Haskell graph walker using STM and async concurrency"
    , knowledgeEntryRowOriginalContent = Nothing
    , knowledgeEntryRowMetadata = "{}"
    , knowledgeEntryRowConfidence = 0.9
    , knowledgeEntryRowIsEdited = False
    , knowledgeEntryRowCreatedAt = t0
    , knowledgeEntryRowUpdatedAt = t0
    , knowledgeEntryRowScannedAt = t0
    }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Eva.Knowledge.Types — JSON roundtrip" $ do
    it "KnowledgeEntryId" $ property $ roundtrip @KnowledgeEntryId
    it "KnowledgeSourceType" $ property $ roundtrip @KnowledgeSourceType
    it "KnowledgeCategory" $ property $ roundtrip @KnowledgeCategory
    it "KnowledgeEntry" $ property $ roundtrip @KnowledgeEntry
    it "SearchQuery" $ property $ roundtrip @SearchQuery
    it "SearchResult" $ property $ roundtrip @SearchResult

  describe "Eva.Knowledge.Types — enum serialization" $ do
    describe "KnowledgeSourceType" $ do
      it "SourceCodebase -> \"codebase\"" $ encode SourceCodebase `shouldBe` "\"codebase\""
      it "SourceLinear   -> \"linear\""   $ encode SourceLinear   `shouldBe` "\"linear\""
      it "SourceGitHub   -> \"github\""   $ encode SourceGitHub   `shouldBe` "\"github\""
      it "SourceHttp     -> \"http\""     $ encode SourceHttp     `shouldBe` "\"http\""
      it "SourceManual   -> \"manual\""   $ encode SourceManual   `shouldBe` "\"manual\""
    describe "KnowledgeCategory" $ do
      it "CategoryStructure  -> \"structure\""  $ encode CategoryStructure  `shouldBe` "\"structure\""
      it "CategoryMetadata   -> \"metadata\""   $ encode CategoryMetadata   `shouldBe` "\"metadata\""
      it "CategoryPattern    -> \"pattern\""    $ encode CategoryPattern    `shouldBe` "\"pattern\""
      it "CategorySummary    -> \"summary\""    $ encode CategorySummary    `shouldBe` "\"summary\""
      it "CategoryReference  -> \"reference\""  $ encode CategoryReference  `shouldBe` "\"reference\""

  describe "Eva.Knowledge.Types — field serialization" $ do
    it "KnowledgeEntry strips 'knowledgeEntry' prefix" $ do
      let entry = KnowledgeEntry
            { knowledgeEntryId = "ke-1"
            , knowledgeEntrySourceType = SourceCodebase
            , knowledgeEntrySourceId = Just "src/Main.hs"
            , knowledgeEntryProgramId = Nothing
            , knowledgeEntryCategory = CategoryStructure
            , knowledgeEntryTitle = "Main module"
            , knowledgeEntryContent = "Entry point"
            , knowledgeEntryOriginalContent = Nothing
            , knowledgeEntryMetadata = object []
            , knowledgeEntryConfidence = 1.0
            , knowledgeEntryIsEdited = False
            , knowledgeEntryCreatedAt = t0
            , knowledgeEntryUpdatedAt = t0
            , knowledgeEntryScannedAt = t0
            }
      let obj = toJSON entry
      case obj of
        Object km -> do
          KM.member "id" km `shouldBe` True
          KM.member "sourceType" km `shouldBe` True
          KM.member "knowledgeEntrySourceType" km `shouldBe` False
        _ -> expectationFailure "Expected JSON object"

    it "omitNothingFields: originalContent absent when Nothing" $ do
      let entry = KnowledgeEntry
            { knowledgeEntryId = "ke-2"
            , knowledgeEntrySourceType = SourceManual
            , knowledgeEntrySourceId = Nothing
            , knowledgeEntryProgramId = Nothing
            , knowledgeEntryCategory = CategorySummary
            , knowledgeEntryTitle = "Note"
            , knowledgeEntryContent = "Some content"
            , knowledgeEntryOriginalContent = Nothing
            , knowledgeEntryMetadata = object []
            , knowledgeEntryConfidence = 0.5
            , knowledgeEntryIsEdited = False
            , knowledgeEntryCreatedAt = t0
            , knowledgeEntryUpdatedAt = t0
            , knowledgeEntryScannedAt = t0
            }
      let obj = toJSON entry
      case obj of
        Object km -> KM.member "originalContent" km `shouldBe` False
        _ -> expectationFailure "Expected JSON object"

    it "backward-compatible: JSON without originalContent decodes as Nothing" $ do
      let json =
            "{\"id\":\"ke-3\",\"sourceType\":\"manual\",\"category\":\"summary\",\
            \\"title\":\"T\",\"content\":\"C\",\"metadata\":{},\
            \\"confidence\":0.5,\"isEdited\":false,\
            \\"createdAt\":\"2025-02-20T00:00:00Z\",\
            \\"updatedAt\":\"2025-02-20T00:00:00Z\",\
            \\"scannedAt\":\"2025-02-20T00:00:00Z\"}"
      case eitherDecode json :: Either String KnowledgeEntry of
        Left err -> expectationFailure $ "Decode failed: " <> err
        Right entry -> knowledgeEntryOriginalContent entry `shouldBe` Nothing

  describe "Eva.Knowledge.Types — FTS5 integration" $ do
    it "INSERT trigger: entry is searchable after insert" $
      withTestPool $ \pool -> do
        runSqlPool (insertKey sampleKey sampleRow) pool
        rows <- runSqlPool
          (rawSql
            "SELECT rowid FROM knowledge_fts WHERE knowledge_fts MATCH ?"
            [PersistText "haskell"])
          pool
        length (rows :: [Single Int]) `shouldBe` 1

    it "DELETE trigger: entry is no longer searchable after delete" $
      withTestPool $ \pool -> do
        runSqlPool (insertKey sampleKey sampleRow) pool
        runSqlPool (delete sampleKey) pool
        rows <- runSqlPool
          (rawSql
            "SELECT rowid FROM knowledge_fts WHERE knowledge_fts MATCH ?"
            [PersistText "haskell"])
          pool
        length (rows :: [Single Int]) `shouldBe` 0

    it "UPDATE trigger: FTS reflects updated content" $
      withTestPool $ \pool -> do
        runSqlPool (insertKey sampleKey sampleRow) pool
        runSqlPool
          (update sampleKey [KnowledgeEntryRowContent =. "Erlang actor model with OTP supervision trees"])
          pool
        rowsOld <- runSqlPool
          (rawSql
            "SELECT rowid FROM knowledge_fts WHERE knowledge_fts MATCH ?"
            [PersistText "haskell"])
          pool
        rowsNew <- runSqlPool
          (rawSql
            "SELECT rowid FROM knowledge_fts WHERE knowledge_fts MATCH ?"
            [PersistText "erlang"])
          pool
        length (rowsOld :: [Single Int]) `shouldBe` 0
        length (rowsNew :: [Single Int]) `shouldBe` 1
