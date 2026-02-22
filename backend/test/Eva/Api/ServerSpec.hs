{-# LANGUAGE OverloadedStrings #-}

module Eva.Api.ServerSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap ((!?))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.Persist.Sqlite (createSqlitePool)
import Network.HTTP.Types
  ( hContentType
  , methodDelete
  , methodGet
  , methodPatch
  , methodPost
  , methodPut
  , status200
  , status201
  , status204
  , status404
  , status409
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

import Eva.Api.Server (makeApp)
import Eva.App (AppEnv (..))
import Eva.Config (AppConfig (..), LogLevel (..))
import Eva.Persistence.Migration (runMigrations)

-- ---------------------------------------------------------------------------
-- Test harness
-- ---------------------------------------------------------------------------

-- | Build a fresh in-memory application. Called via 'before' so each test
-- gets an isolated SQLite database.
makeTestApp :: IO Application
makeTestApp = do
  pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
  runMigrations pool
  let env = AppEnv
        { envConfig = AppConfig
            { configDbPath    = ":memory:"
            , configPort      = 8080
            , configLlmApiKey = Nothing
            , configLogLevel  = LogError
            }
        , envDbPool = pool
        , envLogger = \_ -> pure ()
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

doPutJson :: BL.ByteString -> Value -> Session SResponse
doPutJson path body = srequest SRequest
  { simpleRequest = setPath
      defaultRequest
        { requestMethod  = methodPut
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
-- Response decode helpers
-- ---------------------------------------------------------------------------

-- | Extract the "id" field from a JSON object response body (as a path segment).
responseId :: SResponse -> Maybe BL.ByteString
responseId res = do
  Object obj <- decode (simpleBody res)
  String t   <- obj !? "id"
  pure (BL.fromStrict (TE.encodeUtf8 t))

-- | Extract a named text field from a JSON object response body.
responseField :: Text -> SResponse -> Maybe Text
responseField key res = do
  Object obj <- decode (simpleBody res)
  String t   <- obj !? fromText key
  pure t

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = before makeTestApp $ do

  describe "GET /api/health" $ do
    it "returns 200 with status ok" $ \app -> sess app $ do
      res <- doGet "/api/health"
      liftIO $ do
        simpleStatus res `shouldBe` status200
        responseField "status" res `shouldBe` Just "ok"

  describe "GET /api/programs" $ do
    it "returns 200 with empty list initially" $ \app -> sess app $ do
      res <- doGet "/api/programs"
      liftIO $ simpleStatus res `shouldBe` status200

  describe "POST /api/programs" $ do
    it "creates a program and returns 201 with draft state" $ \app -> sess app $ do
      res <- doPostJson "/api/programs" (object ["name" .= ("My Program" :: Text)])
      liftIO $ do
        simpleStatus res `shouldBe` status201
        responseField "name"  res `shouldBe` Just "My Program"
        responseField "state" res `shouldBe` Just "draft"

  describe "GET /api/programs/:id" $ do
    it "returns the program by id" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Fetch Me" :: Text)])
      let Just pid = responseId created
      res <- doGet ("/api/programs/" <> pid)
      liftIO $ do
        simpleStatus res `shouldBe` status200
        responseField "name" res `shouldBe` Just "Fetch Me"

    it "returns 404 for unknown id" $ \app -> sess app $ do
      res <- doGet "/api/programs/no-such-program"
      liftIO $ simpleStatus res `shouldBe` status404

  describe "PATCH /api/programs/:id" $ do
    it "updates the name and returns updated program" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Old Name" :: Text)])
      let Just pid = responseId created
      res <- doPatchJson ("/api/programs/" <> pid) (object ["name" .= ("New Name" :: Text)])
      liftIO $ do
        simpleStatus res `shouldBe` status200
        responseField "name" res `shouldBe` Just "New Name"

    it "returns 404 for unknown id" $ \app -> sess app $ do
      res <- doPatchJson "/api/programs/ghost" (object ["name" .= ("x" :: Text)])
      liftIO $ simpleStatus res `shouldBe` status404

  describe "DELETE /api/programs/:id" $ do
    it "removes program; subsequent GET returns 404" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("To Delete" :: Text)])
      let Just pid = responseId created
      delRes <- doDelete ("/api/programs/" <> pid)
      liftIO $ simpleStatus delRes `shouldBe` status204
      getRes <- doGet ("/api/programs/" <> pid)
      liftIO $ simpleStatus getRes `shouldBe` status404

  describe "PUT /api/programs/:id/graph" $ do
    it "replaces graph and returns 200" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Graph Test" :: Text)])
      let Just pid = responseId created
          emptyGraph = object ["nodes" .= object [], "edges" .= ([] :: [Value])]
      res <- doPutJson ("/api/programs/" <> pid <> "/graph") emptyGraph
      liftIO $ simpleStatus res `shouldBe` status200

    it "returns 404 for unknown program" $ \app -> sess app $ do
      res <- doPutJson "/api/programs/ghost/graph"
        (object ["nodes" .= object [], "edges" .= ([] :: [Value])])
      liftIO $ simpleStatus res `shouldBe` status404

  describe "POST /api/programs/:id/validate" $ do
    it "returns valid=false with error message for empty graph" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Validate Me" :: Text)])
      let Just pid = responseId created
      res <- doPostJson ("/api/programs/" <> pid <> "/validate") (object [])
      liftIO $ do
        simpleStatus res `shouldBe` status200
        let Just (Object body) = decode (simpleBody res) :: Maybe Value
        body !? "valid" `shouldBe` Just (Bool False)

  describe "State transitions" $ do
    it "deploy: Draft → Active (200)" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Deployable" :: Text)])
      let Just pid = responseId created
      res <- doPostJson ("/api/programs/" <> pid <> "/deploy") (object [])
      liftIO $ do
        simpleStatus res `shouldBe` status200
        responseField "state" res `shouldBe` Just "active"

    it "pause: Active → Paused (200)" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Pausable" :: Text)])
      let Just pid = responseId created
      _   <- doPostJson ("/api/programs/" <> pid <> "/deploy") (object [])
      res <- doPostJson ("/api/programs/" <> pid <> "/pause")  (object [])
      liftIO $ do
        simpleStatus res `shouldBe` status200
        responseField "state" res `shouldBe` Just "paused"

    it "resume: Paused → Active (200)" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Resumable" :: Text)])
      let Just pid = responseId created
      _   <- doPostJson ("/api/programs/" <> pid <> "/deploy") (object [])
      _   <- doPostJson ("/api/programs/" <> pid <> "/pause")  (object [])
      res <- doPostJson ("/api/programs/" <> pid <> "/resume") (object [])
      liftIO $ do
        simpleStatus res `shouldBe` status200
        responseField "state" res `shouldBe` Just "active"

    it "deploy on Active program returns 409" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Double Deploy" :: Text)])
      let Just pid = responseId created
      _   <- doPostJson ("/api/programs/" <> pid <> "/deploy") (object [])
      res <- doPostJson ("/api/programs/" <> pid <> "/deploy") (object [])
      liftIO $ simpleStatus res `shouldBe` status409

    it "pause on Draft program returns 409" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Draft Pause" :: Text)])
      let Just pid = responseId created
      res <- doPostJson ("/api/programs/" <> pid <> "/pause") (object [])
      liftIO $ simpleStatus res `shouldBe` status409

    it "resume on Active program returns 409" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Active Resume" :: Text)])
      let Just pid = responseId created
      _   <- doPostJson ("/api/programs/" <> pid <> "/deploy") (object [])
      res <- doPostJson ("/api/programs/" <> pid <> "/resume") (object [])
      liftIO $ simpleStatus res `shouldBe` status409

  describe "CORS headers" $ do
    it "includes Access-Control-Allow-Origin: * on all responses" $ \app -> sess app $ do
      res <- doGet "/api/programs"
      liftIO $
        lookup "Access-Control-Allow-Origin" (simpleHeaders res)
          `shouldBe` Just "*"
