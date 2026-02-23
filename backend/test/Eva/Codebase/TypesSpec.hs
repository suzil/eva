{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eva.Codebase.TypesSpec (spec) where

import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec
import Test.QuickCheck

import Eva.Codebase.Types

-- Bring existing Arbitrary instances for ProgramId, RunId, StepId into scope
-- rather than re-declaring them here.
import Eva.Core.TypesSpec ()

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

-- ---------------------------------------------------------------------------
-- Arbitrary instances — identifiers
-- ---------------------------------------------------------------------------

instance Arbitrary CodebaseId where
  arbitrary = CodebaseId <$> arbitraryText

instance Arbitrary CodeChangesetId where
  arbitrary = CodeChangesetId <$> arbitraryText

instance Arbitrary FileChangeId where
  arbitrary = FileChangeId <$> arbitraryText

-- ---------------------------------------------------------------------------
-- Arbitrary instances — enums
-- ---------------------------------------------------------------------------

instance Arbitrary CodeChangesetStatus where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary FileChangeAction where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary FileChangeStatus where
  arbitrary = elements [minBound .. maxBound]

-- ---------------------------------------------------------------------------
-- Arbitrary instances — records
-- ---------------------------------------------------------------------------

instance Arbitrary FileNode where
  -- Limit recursion depth to keep test sizes manageable.
  arbitrary = sized $ \n ->
    if n <= 0
      then FileNode <$> arbitraryText <*> arbitraryText <*> pure False <*> pure [] <*> (Just <$> choose (0, 100_000))
      else do
        isDir <- arbitrary
        children <-
          if isDir
            then resize (n `div` 3) (listOf arbitrary)
            else pure []
        FileNode
          <$> arbitraryText
          <*> arbitraryText
          <*> pure isDir
          <*> pure children
          <*> if isDir then pure Nothing else Just <$> choose (0, 100_000)

instance Arbitrary FileEntry where
  arbitrary =
    FileEntry
      <$> arbitraryText
      <*> arbitraryText
      <*> arbitraryText
      <*> choose (0, 1_000_000)

instance Arbitrary CodebaseMetadata where
  arbitrary =
    CodebaseMetadata
      <$> arbitrary
      <*> arbitrary
      <*> arbitraryText
      <*> (Map.fromList <$> listOf ((,) <$> arbitraryText <*> choose (0, 500)))
      <*> listOf arbitraryText
      <*> oneof [pure Nothing, Just <$> arbitraryText]
      <*> arbitrary
      <*> arbitraryUTCTime

instance Arbitrary FileChange where
  arbitrary =
    FileChange
      <$> arbitrary
      <*> arbitrary
      <*> arbitraryText
      <*> arbitrary
      <*> oneof [pure Nothing, Just <$> arbitraryText]
      <*> arbitraryText
      <*> arbitrary

instance Arbitrary CodeChangeset where
  arbitrary =
    CodeChangeset
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> listOf arbitrary
      <*> arbitraryUTCTime

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Eva.Codebase.Types" $ do
  describe "JSON roundtrip — identifiers" $ do
    it "CodebaseId" $ property $ roundtrip @CodebaseId
    it "CodeChangesetId" $ property $ roundtrip @CodeChangesetId
    it "FileChangeId" $ property $ roundtrip @FileChangeId

  describe "JSON roundtrip — enums" $ do
    it "CodeChangesetStatus" $ property $ roundtrip @CodeChangesetStatus
    it "FileChangeAction" $ property $ roundtrip @FileChangeAction
    it "FileChangeStatus" $ property $ roundtrip @FileChangeStatus

  describe "JSON roundtrip — records" $ do
    it "FileNode" $ property $ roundtrip @FileNode
    it "FileEntry" $ property $ roundtrip @FileEntry
    it "CodebaseMetadata" $ property $ roundtrip @CodebaseMetadata
    it "FileChange" $ property $ roundtrip @FileChange
    it "CodeChangeset" $ property $ roundtrip @CodeChangeset

  describe "enum serialization — CodeChangesetStatus" $ do
    it "ChangesetPending  -> \"pending\""  $ encode ChangesetPending  `shouldBe` "\"pending\""
    it "ChangesetApplied  -> \"applied\""  $ encode ChangesetApplied  `shouldBe` "\"applied\""
    it "ChangesetRejected -> \"rejected\"" $ encode ChangesetRejected `shouldBe` "\"rejected\""

  describe "enum serialization — FileChangeAction" $ do
    it "FileActionAdd    -> \"add\""    $ encode FileActionAdd    `shouldBe` "\"add\""
    it "FileActionModify -> \"modify\"" $ encode FileActionModify `shouldBe` "\"modify\""
    it "FileActionDelete -> \"delete\"" $ encode FileActionDelete `shouldBe` "\"delete\""

  describe "enum serialization — FileChangeStatus" $ do
    it "FileChangePending  -> \"pending\""  $ encode FileChangePending  `shouldBe` "\"pending\""
    it "FileChangeAccepted -> \"accepted\"" $ encode FileChangeAccepted `shouldBe` "\"accepted\""
    it "FileChangeRejected -> \"rejected\"" $ encode FileChangeRejected `shouldBe` "\"rejected\""

  describe "field serialization" $ do
    it "FileNode fields strip 'fileNode' prefix" $ do
      let node = FileNode
            { fileNodeName = "src"
            , fileNodePath = "/project/src"
            , fileNodeIsDir = True
            , fileNodeChildren = []
            , fileNodeSize = Nothing
            }
      let obj = toJSON node
      obj `shouldBe` object
        [ "name"     .= ("src" :: Text)
        , "path"     .= ("/project/src" :: Text)
        , "isDir"    .= True
        , "children" .= ([] :: [FileNode])
        ]

    it "FileChange fields strip 'fileChange' prefix" $ do
      let fc = FileChange
            { fileChangeId = "fc-1"
            , fileChangeChangesetId = "cs-1"
            , fileChangePath = "src/Main.hs"
            , fileChangeAction = FileActionModify
            , fileChangeOriginalContent = Just "old"
            , fileChangeProposedContent = "new"
            , fileChangeStatus = FileChangePending
            }
      let obj = toJSON fc
      obj `shouldBe` object
        [ "id"              .= ("fc-1" :: Text)
        , "changesetId"     .= ("cs-1" :: Text)
        , "path"            .= ("src/Main.hs" :: Text)
        , "action"          .= ("modify" :: Text)
        , "originalContent" .= ("old" :: Text)
        , "proposedContent" .= ("new" :: Text)
        , "status"          .= ("pending" :: Text)
        ]
