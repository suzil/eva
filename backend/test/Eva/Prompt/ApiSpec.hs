{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for EVA-100: Template REST endpoints.
module Eva.Prompt.ApiSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap ((!?))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
  ( hContentType
  , methodDelete
  , methodGet
  , methodPatch
  , methodPost
  , status200
  , status201
  , status204
  , status403
  , status404
  )
import Network.Wai (Application, requestHeaders, requestMethod)
import Network.Wai.Test
  ( SRequest (..)
  , SResponse (..)
  , Session
  , defaultRequest
  , runSession
  , setPath
  , srequest
  )
import Test.Hspec

import Control.Monad.Logger (runNoLoggingT)
import Database.Persist.Sqlite (createSqlitePool)

import Eva.Api.Server (makeApp)
import Eva.App (AppEnv (..))
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Engine.Dispatch (execute)
import Eva.Engine.LLM (dummyLLMClient)
import Eva.Persistence.Migration (runMigrations)
import Eva.Prompt.Store (seedBuiltinTemplates)

-- ---------------------------------------------------------------------------
-- Test harness
-- ---------------------------------------------------------------------------

makeTestApp :: IO Application
makeTestApp = do
  pool       <- runNoLoggingT $ createSqlitePool ":memory:" 1
  runMigrations pool
  seedBuiltinTemplates pool
  broadcasts <- newTVarIO Map.empty
  let env = AppEnv
        { envConfig = AppConfig
            { configDbPath          = ":memory:"
            , configPort            = 8080
            , configLlmApiKey       = Nothing
            , configAnthropicApiKey = Nothing
            , configLogLevel        = LogError
            , configCredentialKey   = "test-key"
            , configStaticDir       = Nothing
            }
        , envDbPool          = pool
        , envLogger          = \_ -> pure ()
        , envDispatch        = execute
        , envLLMClient       = dummyLLMClient
        , envAnthropicClient = dummyLLMClient
        , envBroadcasts      = broadcasts
        , envCredentialKey   = Crypto.deriveKey "test-key"
        }
  pure (makeApp env)

sess :: Application -> Session a -> IO a
sess app s = runSession s app

-- ---------------------------------------------------------------------------
-- Request helpers
-- ---------------------------------------------------------------------------

doGet :: BL.ByteString -> Session SResponse
doGet path = srequest SRequest
  { simpleRequest     = setPath defaultRequest { requestMethod = methodGet } (BL.toStrict path)
  , simpleRequestBody = ""
  }

doDelete :: BL.ByteString -> Session SResponse
doDelete path = srequest SRequest
  { simpleRequest     = setPath defaultRequest { requestMethod = methodDelete } (BL.toStrict path)
  , simpleRequestBody = ""
  }

doPostJson :: BL.ByteString -> Value -> Session SResponse
doPostJson path body = srequest SRequest
  { simpleRequest = setPath
      defaultRequest
        { requestMethod  = methodPost
        , requestHeaders = [(hContentType, "application/json")]
        }
      (BL.toStrict path)
  , simpleRequestBody = encode body
  }

doPatchJson :: BL.ByteString -> Value -> Session SResponse
doPatchJson path body = srequest SRequest
  { simpleRequest = setPath
      defaultRequest
        { requestMethod  = methodPatch
        , requestHeaders = [(hContentType, "application/json")]
        }
      (BL.toStrict path)
  , simpleRequestBody = encode body
  }

-- ---------------------------------------------------------------------------
-- Response helpers
-- ---------------------------------------------------------------------------

extractField :: Text -> SResponse -> Maybe Text
extractField field res = do
  Object obj <- decode (simpleBody res)
  String t   <- obj !? fromText field
  pure t

extractId :: SResponse -> Maybe BL.ByteString
extractId res = BL.fromStrict . TE.encodeUtf8 <$> extractField "id" res

-- | Create a user template and return its ID as a URL-safe ByteString.
createUserTemplate :: Application -> IO BL.ByteString
createUserTemplate app = do
  res <- sess app $ doPostJson "/api/templates" $ object
    [ "name"        .= ("My Template" :: Text)
    , "description" .= ("A test template" :: Text)
    , "category"    .= ("custom" :: Text)
    , "tags"        .= ([] :: [Text])
    , "body"        .= ("Hello {{input}}" :: Text)
    , "variables"   .= ([] :: [Value])
    ]
  case extractId res of
    Just tid -> pure ("/api/templates/" <> tid)
    Nothing  -> fail "createUserTemplate: expected id in response"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = before makeTestApp $ do

  describe "GET /api/templates" $ do
    it "AC1: returns 7 built-in templates on a fresh database" $ \app -> do
      res <- sess app $ doGet "/api/templates"
      simpleStatus res `shouldBe` status200
      case decode (simpleBody res) of
        Just (Array xs) -> length xs `shouldBe` 7
        _               -> expectationFailure "expected a JSON array"

  describe "DELETE /api/templates/:id" $ do
    it "AC2: returns 403 for a built-in template" $ \app -> do
      res <- sess app $ doDelete "/api/templates/tmpl-summarizer"
      simpleStatus res `shouldBe` status403

    it "returns 204 for a user template" $ \app -> do
      url <- createUserTemplate app
      res <- sess app $ doDelete url
      simpleStatus res `shouldBe` status204

    it "returns 404 for an unknown template" $ \app -> do
      res <- sess app $ doDelete "/api/templates/does-not-exist"
      simpleStatus res `shouldBe` status404

  describe "POST /api/templates" $ do
    it "returns 201 with the created template; builtIn is false" $ \app -> do
      res <- sess app $ doPostJson "/api/templates" $ object
        [ "name"        .= ("Custom" :: Text)
        , "description" .= ("desc" :: Text)
        , "category"    .= ("custom" :: Text)
        , "tags"        .= ([] :: [Text])
        , "body"        .= ("{{input}}" :: Text)
        , "variables"   .= ([] :: [Value])
        ]
      simpleStatus res `shouldBe` status201
      case decode (simpleBody res) of
        Just (Object obj) -> do
          obj !? "name"    `shouldBe` Just (String "Custom")
          obj !? "builtIn" `shouldBe` Just (Bool False)
          obj !? "id"      `shouldSatisfy` (/= Nothing)
        _ -> expectationFailure "expected a JSON object"

  describe "PATCH /api/templates/:id" $ do
    it "AC3: updates body and variables for a user template" $ \app -> do
      url  <- createUserTemplate app
      let newVars = [ object
                        [ "name"        .= ("x" :: Text)
                        , "description" .= ("desc" :: Text)
                        , "required"    .= True
                        ]
                    ]
      res <- sess app $ doPatchJson url $ object
        [ "body"      .= ("Updated {{x}}" :: Text)
        , "variables" .= newVars
        ]
      simpleStatus res `shouldBe` status200
      case decode (simpleBody res) of
        Just (Object obj) -> do
          obj !? "body" `shouldBe` Just (String "Updated {{x}}")
          obj !? "name" `shouldBe` Just (String "My Template")
        _ -> expectationFailure "expected a JSON object"

    it "returns 403 when patching a built-in template" $ \app -> do
      res <- sess app $ doPatchJson "/api/templates/tmpl-summarizer" $
        object ["body" .= ("changed" :: Text)]
      simpleStatus res `shouldBe` status403

    it "returns 404 for an unknown template" $ \app -> do
      res <- sess app $ doPatchJson "/api/templates/does-not-exist" $
        object ["body" .= ("x" :: Text)]
      simpleStatus res `shouldBe` status404
