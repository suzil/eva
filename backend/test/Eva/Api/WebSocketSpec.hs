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
  , isTerminalRunState
  , parseRunIdFromTopic
  , runStateEvent
  , stepStateEvent
  )
import Eva.Core.Types (RunId (..), RunState (..), StepId (..), StepState (..))

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "parseRunIdFromTopic" $ do
    it "extracts RunId from 'run:<id>'" $
      parseRunIdFromTopic "run:abc-123" `shouldBe` Just (RunId "abc-123")

    it "returns Nothing for empty id after prefix" $
      parseRunIdFromTopic "run:" `shouldBe` Nothing

    it "returns Nothing for unrecognised prefix" $
      parseRunIdFromTopic "step:abc" `shouldBe` Nothing

    it "returns Nothing for empty string" $
      parseRunIdFromTopic "" `shouldBe` Nothing

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
