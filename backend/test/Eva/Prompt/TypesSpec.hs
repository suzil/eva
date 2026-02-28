{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eva.Prompt.TypesSpec (spec) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec
import Test.QuickCheck

import Eva.Prompt.Types

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
-- Arbitrary instances
-- ---------------------------------------------------------------------------

instance Arbitrary TemplateId where
  arbitrary = TemplateId <$> arbitraryText

instance Arbitrary TemplateCategory where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary TemplateVariable where
  arbitrary =
    TemplateVariable
      <$> arbitraryText
      <*> arbitraryText
      <*> arbitrary
      <*> oneof [pure Nothing, Just <$> arbitraryText]

instance Arbitrary PromptTemplate where
  arbitrary =
    PromptTemplate
      <$> arbitrary
      <*> arbitraryText
      <*> arbitraryText
      <*> arbitrary
      <*> listOf arbitraryText
      <*> arbitraryText
      <*> listOf arbitrary
      <*> arbitrary
      <*> arbitraryUTCTime
      <*> arbitraryUTCTime

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Eva.Prompt.Types — JSON roundtrip" $ do
    it "TemplateId"       $ property $ roundtrip @TemplateId
    it "TemplateCategory" $ property $ roundtrip @TemplateCategory
    it "TemplateVariable" $ property $ roundtrip @TemplateVariable
    it "PromptTemplate"   $ property $ roundtrip @PromptTemplate

  describe "Eva.Prompt.Types — TemplateCategory serialization" $ do
    it "Summarizer  -> \"summarizer\""  $ encode Summarizer  `shouldBe` "\"summarizer\""
    it "Reviewer    -> \"reviewer\""    $ encode Reviewer    `shouldBe` "\"reviewer\""
    it "Classifier  -> \"classifier\""  $ encode Classifier  `shouldBe` "\"classifier\""
    it "Extractor   -> \"extractor\""   $ encode Extractor   `shouldBe` "\"extractor\""
    it "Formatter   -> \"formatter\""   $ encode Formatter   `shouldBe` "\"formatter\""
    it "Analyst     -> \"analyst\""     $ encode Analyst     `shouldBe` "\"analyst\""
    it "Custom      -> \"custom\""      $ encode Custom      `shouldBe` "\"custom\""

  describe "Eva.Prompt.Types — PromptTemplate field prefix stripping" $ do
    it "strips 'promptTemplate' prefix: 'name' key present" $ do
      let tmpl = sampleTemplate
      case toJSON tmpl of
        Object km -> KM.member "name" km `shouldBe` True
        _         -> expectationFailure "Expected JSON object"
    it "strips 'promptTemplate' prefix: 'promptTemplateName' key absent" $ do
      let tmpl = sampleTemplate
      case toJSON tmpl of
        Object km -> KM.member "promptTemplateName" km `shouldBe` False
        _         -> expectationFailure "Expected JSON object"

  describe "Eva.Prompt.Types — TemplateVariable field prefix stripping" $ do
    it "strips 'templateVariable' prefix: 'name' key present" $ do
      let v = TemplateVariable "myVar" "a description" True Nothing
      case toJSON v of
        Object km -> KM.member "name" km `shouldBe` True
        _         -> expectationFailure "Expected JSON object"

  describe "Eva.Prompt.Types — omitNothingFields" $ do
    it "defaultValue absent from JSON when Nothing" $ do
      let v = TemplateVariable "x" "desc" True Nothing
      case toJSON v of
        Object km -> KM.member "defaultValue" km `shouldBe` False
        _         -> expectationFailure "Expected JSON object"
    it "defaultValue present in JSON when Just" $ do
      let v = TemplateVariable "x" "desc" False (Just "fallback")
      case toJSON v of
        Object km -> KM.member "defaultValue" km `shouldBe` True
        _         -> expectationFailure "Expected JSON object"

  describe "Eva.Prompt.Types — TemplateVariable validity" $ do
    it "required=True with no defaultValue is a valid value" $ do
      let v = TemplateVariable "input" "Required input" True Nothing
      templateVariableRequired v `shouldBe` True
      templateVariableDefaultValue v `shouldBe` Nothing

  describe "Eva.Prompt.Types — built-in template bodies" $ do
    it "builtinSummarizerBody contains {{content}}" $
      T.isInfixOf "{{content}}" builtinSummarizerBody `shouldBe` True
    it "builtinCodeReviewerBody contains {{code}}" $
      T.isInfixOf "{{code}}" builtinCodeReviewerBody `shouldBe` True
    it "builtinIssueClassifierBody contains {{issue}}" $
      T.isInfixOf "{{issue}}" builtinIssueClassifierBody `shouldBe` True
    it "builtinDataExtractorBody contains {{text}} and {{schema}}" $ do
      T.isInfixOf "{{text}}"   builtinDataExtractorBody `shouldBe` True
      T.isInfixOf "{{schema}}" builtinDataExtractorBody `shouldBe` True
    it "builtinReportFormatterBody contains {{content}}" $
      T.isInfixOf "{{content}}" builtinReportFormatterBody `shouldBe` True
    it "builtinMeetingNotesAnalystBody contains {{notes}}" $
      T.isInfixOf "{{notes}}" builtinMeetingNotesAnalystBody `shouldBe` True
    it "builtinCustomBody is exactly {{input}}" $
      builtinCustomBody `shouldBe` "{{input}}"

-- ---------------------------------------------------------------------------
-- Sample value
-- ---------------------------------------------------------------------------

sampleTemplate :: PromptTemplate
sampleTemplate =
  PromptTemplate
    { promptTemplateId = "tmpl-summarizer"
    , promptTemplateName = "Summarizer"
    , promptTemplateDescription = "Summarize content concisely"
    , promptTemplateCategory = Summarizer
    , promptTemplateTags = ["summary", "text"]
    , promptTemplateBody = builtinSummarizerBody
    , promptTemplateVariables =
        [ TemplateVariable "content" "Content to summarize" True Nothing
        ]
    , promptTemplateBuiltIn = True
    , promptTemplateCreatedAt = t0
    , promptTemplateUpdatedAt = t0
    }
