{-# LANGUAGE OverloadedStrings #-}

module Eva.Api.WebSocketSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Test.Hspec

import Eva.Api.WebSocket
  ( SubscribeMsg (..)
  , Topic (..)
  , assistantReplyEvent
  , assistantTokenEvent
  , codeChangeEvent
  , isTerminalRunState
  , parseTopic
  , parseRunIdFromTopic
  , runStateEvent
  , stepStateEvent
  )
import Eva.Assistant (AssistantMessage (..), ConversationId (..))
import Eva.Codebase.Types (CodeChangesetId (..))
import Eva.Core.Types (RunId (..), RunState (..), StepId (..), StepState (..))

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "parseTopic" $ do
    it "parses run:<id> into RunTopic" $
      parseTopic "run:abc-123" `shouldBe` Just (RunTopic (RunId "abc-123"))

    it "parses assistant:<id> into AssistantTopic" $
      parseTopic "assistant:conv-42" `shouldBe` Just (AssistantTopic (ConversationId "conv-42"))

    it "returns Nothing for empty run id" $
      parseTopic "run:" `shouldBe` Nothing

    it "returns Nothing for empty assistant id" $
      parseTopic "assistant:" `shouldBe` Nothing

    it "returns Nothing for unrecognised prefix" $
      parseTopic "step:abc" `shouldBe` Nothing

    it "returns Nothing for empty string" $
      parseTopic "" `shouldBe` Nothing

  describe "parseRunIdFromTopic" $ do
    it "extracts RunId from 'run:<id>'" $
      parseRunIdFromTopic "run:abc-123" `shouldBe` Just (RunId "abc-123")

    it "returns Nothing for empty id after prefix" $
      parseRunIdFromTopic "run:" `shouldBe` Nothing

    it "returns Nothing for unrecognised prefix" $
      parseRunIdFromTopic "step:abc" `shouldBe` Nothing

    it "returns Nothing for empty string" $
      parseRunIdFromTopic "" `shouldBe` Nothing

    it "returns Nothing for assistant topic" $
      parseRunIdFromTopic "assistant:conv-1" `shouldBe` Nothing

  describe "isTerminalRunState" $ do
    let mkRunState s = object ["type" .= ("run_state" :: Text), "state" .= (s :: Text)]

    it "returns True for completed" $
      isTerminalRunState (mkRunState "completed") `shouldBe` True

    it "returns True for failed" $
      isTerminalRunState (mkRunState "failed") `shouldBe` True

    it "returns True for canceled" $
      isTerminalRunState (mkRunState "canceled") `shouldBe` True

    it "returns False for running" $
      isTerminalRunState (mkRunState "running") `shouldBe` False

    it "returns False for non-run_state type" $
      isTerminalRunState (object ["type" .= ("step_state" :: Text), "state" .= ("completed" :: Text)])
        `shouldBe` False

    it "returns False for non-object" $
      isTerminalRunState (Aeson.String "completed") `shouldBe` False

  describe "event constructors" $ do
    it "runStateEvent includes required fields" $ do
      now <- getCurrentTime
      let ev = runStateEvent (RunId "r1") RunCompleted now
      case ev of
        Object o -> do
          KM.lookup "type"  o `shouldBe` Just (Aeson.String "run_state")
          KM.lookup "runId" o `shouldBe` Just (Aeson.String "r1")
        _ -> expectationFailure "expected Object"

    it "stepStateEvent includes required fields" $ do
      now <- getCurrentTime
      let ev = stepStateEvent (RunId "r1") "n1" (StepId "s1") StepRunning now
      case ev of
        Object o -> do
          KM.lookup "type"   o `shouldBe` Just (Aeson.String "step_state")
          KM.lookup "runId"  o `shouldBe` Just (Aeson.String "r1")
          KM.lookup "nodeId" o `shouldBe` Just (Aeson.String "n1")
          KM.lookup "stepId" o `shouldBe` Just (Aeson.String "s1")
        _ -> expectationFailure "expected Object"

    it "codeChangeEvent includes required fields" $ do
      now <- getCurrentTime
      let ev = codeChangeEvent (RunId "r1") (CodeChangesetId "cs1") 3 now
      case ev of
        Object o -> do
          KM.lookup "type"        o `shouldBe` Just (Aeson.String "code_change_event")
          KM.lookup "runId"       o `shouldBe` Just (Aeson.String "r1")
          KM.lookup "changesetId" o `shouldBe` Just (Aeson.String "cs1")
          KM.lookup "fileCount"   o `shouldBe` Just (Aeson.Number 3)
        _ -> expectationFailure "expected Object"

    it "assistantTokenEvent includes required fields" $ do
      now <- getCurrentTime
      let ev = assistantTokenEvent (ConversationId "conv-1") "Hello" now
      case ev of
        Object o -> do
          KM.lookup "type"           o `shouldBe` Just (Aeson.String "assistant_token")
          KM.lookup "conversationId" o `shouldBe` Just (Aeson.String "conv-1")
          KM.lookup "token"          o `shouldBe` Just (Aeson.String "Hello")
        _ -> expectationFailure "expected Object"

    it "assistantReplyEvent includes required fields" $ do
      now <- getCurrentTime
      let ev = assistantReplyEvent (ConversationId "conv-1") (AsstText "done") now
      case ev of
        Object o -> do
          KM.lookup "type"           o `shouldBe` Just (Aeson.String "assistant_reply")
          KM.lookup "conversationId" o `shouldBe` Just (Aeson.String "conv-1")
          case KM.lookup "message" o of
            Just (Object msgObj) ->
              KM.lookup "type" msgObj `shouldBe` Just (Aeson.String "text")
            _ -> expectationFailure "expected message Object"
        _ -> expectationFailure "expected Object"
