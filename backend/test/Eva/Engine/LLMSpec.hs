{-# LANGUAGE OverloadedStrings #-}

module Eva.Engine.LLMSpec (spec) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Eva.Core.Types (ResponseFormat (..))
import Eva.Engine.LLM
  ( ChatMessage (..)
  , LLMClient (..)
  , LLMError (..)
  , LLMRequest (..)
  , classifyStatus
  , dummyLLMClient
  , parseSseLine
  )

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

req :: LLMRequest
req = LLMRequest
  { llmModel          = "gpt-4o"
  , llmMessages       = [ChatMessage "user" "hello"]
  , llmTemperature    = 0.7
  , llmMaxTokens      = Nothing
  , llmResponseFormat = ResponseText
  }

emptyBody :: BL.ByteString
emptyBody = ""

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "parseSseLine" $ do
    it "parses a data line into a JSON value" $ do
      let json = "{\"id\":\"x\",\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}" :: Text
      parseSseLine ("data: " <> json) `shouldBe` decode (BL.fromStrict (TE.encodeUtf8 json))

    it "returns Nothing for [DONE]" $
      parseSseLine "data: [DONE]" `shouldBe` Nothing

    it "returns Nothing for a blank line" $
      parseSseLine "" `shouldBe` Nothing

    it "returns Nothing for a whitespace-only line" $
      parseSseLine "   " `shouldBe` Nothing

    it "returns Nothing for a comment line (no data: prefix)" $
      parseSseLine ": keep-alive" `shouldBe` Nothing

    it "returns Nothing for malformed JSON after data:" $
      parseSseLine "data: {not valid json" `shouldBe` Nothing

    it "handles extra whitespace after data:" $ do
      let json = "{\"choices\":[]}" :: Text
      parseSseLine ("data:   " <> json) `shouldBe` decode (BL.fromStrict (TE.encodeUtf8 json))

  describe "classifyStatus" $ do
    it "returns Right () for 200" $
      classifyStatus 200 emptyBody `shouldBe` Right ()

    it "returns LLMAuthError for 401" $
      classifyStatus 401 emptyBody `shouldSatisfy` isAuthError

    it "returns LLMAuthError for 403" $
      classifyStatus 403 emptyBody `shouldSatisfy` isAuthError

    it "returns LLMRateLimitError for 429" $
      classifyStatus 429 emptyBody `shouldSatisfy` isRateLimitError

    it "returns LLMApiError for 500" $
      classifyStatus 500 emptyBody `shouldSatisfy` isApiError 500

    it "returns LLMApiError for 400 with status code preserved" $
      classifyStatus 400 emptyBody `shouldSatisfy` isApiError 400

    it "extracts error.message from OpenAI error body" $ do
      let body = BLC.pack "{\"error\":{\"message\":\"Invalid API key\",\"type\":\"invalid_request_error\"}}"
      classifyStatus 401 body `shouldBe` Left (LLMAuthError "Invalid API key")

    it "falls back to body excerpt when error.message absent" $ do
      let body = BLC.pack "{\"msg\":\"something went wrong\"}"
      case classifyStatus 500 body of
        Left (LLMApiError 500 txt) -> txt `shouldSatisfy` T.isInfixOf "something"
        other                       -> expectationFailure ("unexpected: " <> show other)

  describe "dummyLLMClient" $ do
    it "clientCall returns LLMAuthError" $ do
      result <- clientCall dummyLLMClient req
      result `shouldBe` Left (LLMAuthError "no LLM client configured")

    it "clientStream returns LLMAuthError without firing callback" $ do
      let callbackBoom :: Text -> IO ()
          callbackBoom _ = expectationFailure "callback should not be called"
      result <- clientStream dummyLLMClient req callbackBoom
      result `shouldBe` Left (LLMAuthError "no LLM client configured")

-- ---------------------------------------------------------------------------
-- Predicate helpers
-- ---------------------------------------------------------------------------

isAuthError :: Either LLMError () -> Bool
isAuthError (Left (LLMAuthError _)) = True
isAuthError _                       = False

isRateLimitError :: Either LLMError () -> Bool
isRateLimitError (Left (LLMRateLimitError _)) = True
isRateLimitError _                            = False

isApiError :: Int -> Either LLMError () -> Bool
isApiError n (Left (LLMApiError code _)) = code == n
isApiError _ _                           = False
