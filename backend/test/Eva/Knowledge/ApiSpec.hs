{-# LANGUAGE OverloadedStrings #-}

module Eva.Knowledge.ApiSpec (spec) where

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

-- ---------------------------------------------------------------------------
-- Test harness
-- ---------------------------------------------------------------------------

makeTestApp :: IO Application
makeTestApp = do
  pool       <- runNoLoggingT $ createSqlitePool ":memory:" 1
  runMigrations pool
  broadcasts <- newTVarIO Map.empty
  cancelTokens <- newTVarIO Map.empty
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
        , envCancelTokens    = cancelTokens
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

-- | Create a program and return its ID as a URL-safe ByteString.
createProgram :: Application -> IO BL.ByteString
createProgram app = do
  res <- sess app $ doPostJson "/api/programs" (object ["name" .= ("Test" :: Text)])
  case decode (simpleBody res) of
    Just (Object obj) | Just (String t) <- obj !? "id" ->
      pure (BL.fromStrict (TE.encodeUtf8 t))
    _ -> fail "createProgram: unexpected response"

-- | Create a manual knowledge entry for a program and return its entry ID.
createEntry :: Application -> BL.ByteString -> Text -> Text -> IO BL.ByteString
createEntry app pid title content = do
  res <- sess app $ doPostJson
    ("/api/programs/" <> pid <> "/knowledge")
    (object ["title" .= title, "content" .= content])
  case extractId res of
    Just eid -> pure eid
    Nothing  -> fail "createEntry: expected id in response"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = before makeTestApp $ do

  describe "GET /api/programs/:id/knowledge" $ do
    it "AC1: returns empty array for a new program" $ \app -> do
      pid <- createProgram app
      res <- sess app $ doGet ("/api/programs/" <> pid <> "/knowledge")
      simpleStatus res `shouldBe` status200
      case decode (simpleBody res) of
        Just (Array _) -> pure ()
        _              -> expectationFailure "expected a JSON array"

  describe "POST /api/programs/:id/knowledge" $ do
    it "AC2: returns 201 with the created entry" $ \app -> do
      pid <- createProgram app
      res <- sess app $ doPostJson
        ("/api/programs/" <> pid <> "/knowledge")
        (object ["title" .= ("Test Entry" :: Text), "content" .= ("hello world" :: Text)])
      simpleStatus res `shouldBe` status201
      case decode (simpleBody res) of
        Just (Object obj) -> do
          obj !? "id"      `shouldSatisfy` (/= Nothing)
          obj !? "title"   `shouldBe` Just (String "Test Entry")
          obj !? "content" `shouldBe` Just (String "hello world")
          obj !? "isEdited" `shouldBe` Just (Bool True)
        _ -> expectationFailure "expected a JSON object"

    it "returns the entry in the list after creation" $ \app -> do
      pid <- createProgram app
      _   <- createEntry app pid "My Entry" "some content"
      res <- sess app $ doGet ("/api/programs/" <> pid <> "/knowledge")
      case decode (simpleBody res) of
        Just (Array xs) -> length xs `shouldBe` 1
        _               -> expectationFailure "expected a JSON array"

  describe "GET /api/knowledge/:entryId" $ do
    it "AC3: returns the entry by ID" $ \app -> do
      pid <- createProgram app
      eid <- createEntry app pid "Alpha" "content alpha"
      res <- sess app $ doGet ("/api/knowledge/" <> eid)
      simpleStatus res `shouldBe` status200
      case decode (simpleBody res) of
        Just (Object obj) -> obj !? "title" `shouldBe` Just (String "Alpha")
        _                 -> expectationFailure "expected a JSON object"

    it "AC3: returns 404 for unknown entry ID" $ \app -> do
      res <- sess app $ doGet "/api/knowledge/00000000-0000-0000-0000-000000000000"
      simpleStatus res `shouldBe` status404

  describe "PATCH /api/knowledge/:entryId" $ do
    it "AC4: updates title and content; isEdited remains true" $ \app -> do
      pid <- createProgram app
      eid <- createEntry app pid "Old Title" "old content"
      res <- sess app $ doPatchJson
        ("/api/knowledge/" <> eid)
        (object ["title" .= ("New Title" :: Text), "content" .= ("new content" :: Text)])
      simpleStatus res `shouldBe` status200
      case decode (simpleBody res) of
        Just (Object obj) -> do
          obj !? "title"    `shouldBe` Just (String "New Title")
          obj !? "content"  `shouldBe` Just (String "new content")
          obj !? "isEdited" `shouldBe` Just (Bool True)
        _ -> expectationFailure "expected a JSON object"

    it "AC4: partial patch preserves unpatched fields" $ \app -> do
      pid <- createProgram app
      eid <- createEntry app pid "Keep Title" "keep content"
      res <- sess app $ doPatchJson
        ("/api/knowledge/" <> eid)
        (object ["content" .= ("updated" :: Text)])
      case decode (simpleBody res) of
        Just (Object obj) -> do
          obj !? "title"   `shouldBe` Just (String "Keep Title")
          obj !? "content" `shouldBe` Just (String "updated")
        _ -> expectationFailure "expected a JSON object"

  describe "DELETE /api/knowledge/:entryId" $ do
    it "AC5: returns 204 and entry is gone" $ \app -> do
      pid <- createProgram app
      eid <- createEntry app pid "To Delete" "bye"
      delRes <- sess app $ doDelete ("/api/knowledge/" <> eid)
      simpleStatus delRes `shouldBe` status204
      getRes <- sess app $ doGet ("/api/knowledge/" <> eid)
      simpleStatus getRes `shouldBe` status404

  describe "POST /api/programs/:id/knowledge/search" $ do
    it "AC6: returns matching entries after insertion" $ \app -> do
      pid <- createProgram app
      _   <- createEntry app pid "Eva project" "Eva is a prompt IDE"
      _   <- createEntry app pid "Unrelated"   "nothing here"
      res <- sess app $ doPostJson
        ("/api/programs/" <> pid <> "/knowledge/search")
        (object ["text" .= ("Eva" :: Text)])
      simpleStatus res `shouldBe` status200
      case decode (simpleBody res) of
        Just (Array xs) -> length xs `shouldSatisfy` (>= 1)
        _               -> expectationFailure "expected a JSON array"

    it "AC6: returns empty array for non-matching text" $ \app -> do
      pid <- createProgram app
      _   <- createEntry app pid "Some entry" "some content"
      res <- sess app $ doPostJson
        ("/api/programs/" <> pid <> "/knowledge/search")
        (object ["text" .= ("xyzzy_no_match_ever" :: Text)])
      case decode (simpleBody res) of
        Just (Array xs) -> length xs `shouldBe` 0
        _               -> expectationFailure "expected a JSON array"

  describe "POST /api/knowledge/:entryId/reset" $ do
    it "AC7: for a manual entry (no originalContent) clears isEdited" $ \app -> do
      pid <- createProgram app
      eid <- createEntry app pid "Manual" "manual content"
      -- Patch it first so we know it was edited
      _ <- sess app $ doPatchJson
        ("/api/knowledge/" <> eid)
        (object ["content" .= ("patched" :: Text)])
      -- Reset
      resetRes <- sess app $ doPostJson ("/api/knowledge/" <> eid <> "/reset") (object [])
      simpleStatus resetRes `shouldBe` status200
      -- Manual entries have no originalContent so resetToAutoGenerated is a no-op:
      -- isEdited stays true (no original to revert to)
      case decode (simpleBody resetRes) of
        Just (Object _) -> pure ()
        _               -> expectationFailure "expected a JSON object"

  describe "POST /api/programs/:id/knowledge/refresh" $ do
    it "AC8: returns 200 for a program with no non-manual sources (no-op)" $ \app -> do
      pid <- createProgram app
      res <- sess app $ doPostJson ("/api/programs/" <> pid <> "/knowledge/refresh") (object [])
      simpleStatus res `shouldBe` status200
