{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for EVA-31: Linear connector implementation.
-- All tests inject a mock 'LinearApiCall' — no real network calls.
module Eva.Integration.LinearSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.Aeson as Aeson
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eva.Core.Types (ConnectorConfig (..), SystemType (..))
import Eva.Integration.Linear (LinearApiCall, mkLinearRunnerWith)
import Eva.Integration.Types

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

testConfig :: ConnectorConfig
testConfig = ConnectorConfig
  { connectorSystem         = SystemLinear
  , connectorCredentialId   = Just "cred-linear"
  , connectorEndpoint       = Nothing
  , connectorScope          = Nothing
  , connectorActionFilter   = []
  }

-- ---------------------------------------------------------------------------
-- Mock helpers
-- ---------------------------------------------------------------------------

-- | A mock API call that always returns the given value as the 'data' payload.
mockSuccess :: Value -> LinearApiCall
mockSuccess v _query _vars = pure (Right v)

-- | A mock API call that always returns the given error.
mockError :: ConnectorError -> LinearApiCall
mockError err _query _vars = pure (Left err)

-- ---------------------------------------------------------------------------
-- Sample 'data' payloads (already-unwrapped, as returned by callLinearGraphQL)
-- ---------------------------------------------------------------------------

issueListData :: Value
issueListData = object
  [ "issues" .= object
      [ "nodes" .= Aeson.Array
          ( pure $ object
              [ "id"         .= ("abc123" :: Text)
              , "identifier" .= ("EVA-99" :: Text)
              , "title"      .= ("Test issue" :: Text)
              , "state"      .= object ["name" .= ("Todo" :: Text)]
              , "url"        .= ("https://linear.app/eva-ide/issue/EVA-99" :: Text)
              ]
          )
      ]
  ]

issueCreateData :: Value
issueCreateData = object
  [ "issueCreate" .= object
      [ "success" .= True
      , "issue"   .= object
          [ "id"         .= ("def456" :: Text)
          , "identifier" .= ("EVA-100" :: Text)
          , "title"      .= ("New issue" :: Text)
          , "url"        .= ("https://linear.app/eva-ide/issue/EVA-100" :: Text)
          ]
      ]
  ]

issueUpdateData :: Value
issueUpdateData = object
  [ "issueUpdate" .= object
      [ "success" .= True
      , "issue"   .= object
          [ "id"         .= ("def456" :: Text)
          , "identifier" .= ("EVA-100" :: Text)
          , "title"      .= ("Updated title" :: Text)
          , "state"      .= object ["name" .= ("In Progress" :: Text)]
          , "url"        .= ("https://linear.app/eva-ide/issue/EVA-100" :: Text)
          ]
      ]
  ]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "linearActions" $ do
    it "exposes exactly 3 actions" $ do
      let runner = mkLinearRunnerWith (mockError (ConnectorApiError "unused")) testConfig
      actions <- connectorAvailableActions runner
      length actions `shouldBe` 3

    it "action names are list_issues, create_issue, update_issue" $ do
      let runner = mkLinearRunnerWith (mockError (ConnectorApiError "unused")) testConfig
      actions <- connectorAvailableActions runner
      let names = sort (map actionSpecName actions)
      names `shouldBe` ["create_issue", "list_issues", "update_issue"]

  describe "list_issues" $ do
    it "returns issue data on success" $ do
      let runner = mkLinearRunnerWith (mockSuccess issueListData) testConfig
          args   = object []
      result <- connectorExecuteAction runner (ActionName "list_issues") args
      case result of
        Left err -> expectationFailure $ "expected Right, got: " <> T.unpack (connectorErrorText err)
        Right _  -> pure ()

    it "passes state filter through in variables" $ do
      capturedVars <- newIORef (Aeson.Null :: Value)
      let apiCall _query vars = do
            writeIORef capturedVars vars
            pure (Right issueListData)
          runner = mkLinearRunnerWith apiCall testConfig
          args   = object ["state" .= ("In Progress" :: Text)]
      _ <- connectorExecuteAction runner (ActionName "list_issues") args
      vars <- readIORef capturedVars
      let encoded = BLC.unpack (encode vars)
      encoded `shouldContain` "In Progress"

  describe "create_issue" $ do
    it "returns created issue on success" $ do
      let runner = mkLinearRunnerWith (mockSuccess issueCreateData) testConfig
          args   = object
            [ "title"  .= ("New issue" :: Text)
            , "teamId" .= ("team-uuid" :: Text)
            ]
      result <- connectorExecuteAction runner (ActionName "create_issue") args
      case result of
        Left err -> expectationFailure $ "expected Right, got: " <> T.unpack (connectorErrorText err)
        Right _  -> pure ()

    it "returns ConnectorApiError when title is missing" $ do
      let runner = mkLinearRunnerWith (mockError (ConnectorApiError "unused")) testConfig
          args   = object ["teamId" .= ("team-uuid" :: Text)]
      result <- connectorExecuteAction runner (ActionName "create_issue") args
      case result of
        Left (ConnectorApiError msg) -> T.unpack msg `shouldContain` "title"
        Left other  -> expectationFailure $ "wrong error: " <> T.unpack (connectorErrorText other)
        Right _     -> expectationFailure "expected Left"

    it "returns ConnectorApiError when teamId is missing" $ do
      let runner = mkLinearRunnerWith (mockError (ConnectorApiError "unused")) testConfig
          args   = object ["title" .= ("Some title" :: Text)]
      result <- connectorExecuteAction runner (ActionName "create_issue") args
      case result of
        Left (ConnectorApiError msg) -> T.unpack msg `shouldContain` "teamId"
        Left other  -> expectationFailure $ "wrong error: " <> T.unpack (connectorErrorText other)
        Right _     -> expectationFailure "expected Left"

  describe "update_issue" $ do
    it "returns updated issue on success" $ do
      let runner = mkLinearRunnerWith (mockSuccess issueUpdateData) testConfig
          args   = object
            [ "issueId" .= ("def456" :: Text)
            , "title"   .= ("Updated title" :: Text)
            ]
      result <- connectorExecuteAction runner (ActionName "update_issue") args
      case result of
        Left err -> expectationFailure $ "expected Right, got: " <> T.unpack (connectorErrorText err)
        Right _  -> pure ()

    it "returns ConnectorApiError when issueId is missing" $ do
      let runner = mkLinearRunnerWith (mockError (ConnectorApiError "unused")) testConfig
          args   = object ["title" .= ("New title" :: Text)]
      result <- connectorExecuteAction runner (ActionName "update_issue") args
      case result of
        Left (ConnectorApiError msg) -> T.unpack msg `shouldContain` "issueId"
        Left other  -> expectationFailure $ "wrong error: " <> T.unpack (connectorErrorText other)
        Right _     -> expectationFailure "expected Left"

  describe "unknown action" $ do
    it "returns ConnectorApiError for unrecognised action names" $ do
      let runner = mkLinearRunnerWith (mockError (ConnectorApiError "unused")) testConfig
          args   = object []
      result <- connectorExecuteAction runner (ActionName "delete_team") args
      case result of
        Left (ConnectorApiError msg) -> T.unpack msg `shouldContain` "unknown"
        Left other  -> expectationFailure $ "wrong error type: " <> T.unpack (connectorErrorText other)
        Right _     -> expectationFailure "expected Left"

  describe "API error propagation" $ do
    it "propagates ConnectorApiError from the API call" $ do
      let runner = mkLinearRunnerWith (mockError (ConnectorApiError "rate limit")) testConfig
          args   = object []
      result <- connectorExecuteAction runner (ActionName "list_issues") args
      case result of
        Left (ConnectorApiError msg) -> T.unpack msg `shouldContain` "rate limit"
        Left other  -> expectationFailure $ "wrong error type: " <> T.unpack (connectorErrorText other)
        Right _     -> expectationFailure "expected Left"

    it "propagates ConnectorInvalidCredential from the API call" $ do
      let runner = mkLinearRunnerWith (mockError (ConnectorInvalidCredential "bad key")) testConfig
          args   = object []
      result <- connectorExecuteAction runner (ActionName "list_issues") args
      case result of
        Left (ConnectorInvalidCredential _) -> pure ()
        Left other  -> expectationFailure $ "wrong error type: " <> T.unpack (connectorErrorText other)
        Right _     -> expectationFailure "expected Left"
