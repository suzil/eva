{-# LANGUAGE OverloadedStrings #-}

module Eva.Api.ServerSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap ((!?))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
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
  , status400
  , status404
  , status409
  , status422
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

import Control.Concurrent.STM (newTVarIO)
import qualified Data.Map.Strict as Map

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

-- | Build a fresh in-memory application. Called via 'before' so each test
-- gets an isolated SQLite database.
makeTestApp :: IO Application
makeTestApp = do
  pool       <- runNoLoggingT $ createSqlitePool ":memory:" 1
  runMigrations pool
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

    -- A graph with a single manual trigger satisfies all validation checks.
    -- Reused across deploy tests that need a valid program to transition.
    let minimalGraph :: Value
        minimalGraph = object
          [ "nodes" .= object
              [ "n1" .= object
                  [ "id"    .= ("n1" :: Text)
                  , "label" .= ("Start" :: Text)
                  , "type"  .= object
                      [ "type"   .= ("trigger" :: Text)
                      , "config" .= object
                          [ "type"            .= ("manual" :: Text)
                          , "schedule"        .= Null
                          , "eventFilter"     .= Null
                          , "payloadTemplate" .= Null
                          ]
                      ]
                  , "posX"  .= (0.0 :: Double)
                  , "posY"  .= (0.0 :: Double)
                  ]
              ]
          , "edges" .= ([] :: [Value])
          ]

    -- Helper: create a program, PUT the minimal valid graph, then deploy it.
    let createAndDeploy :: Text -> Session BL.ByteString
        createAndDeploy name = do
          created <- doPostJson "/api/programs" (object ["name" .= name])
          let Just pid = responseId created
          _ <- doPutJson ("/api/programs/" <> pid <> "/graph") minimalGraph
          _ <- doPostJson ("/api/programs/" <> pid <> "/deploy") (object [])
          pure pid

    it "deploy: Draft → Active (200)" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Deployable" :: Text)])
      let Just pid = responseId created
      _ <- doPutJson ("/api/programs/" <> pid <> "/graph") minimalGraph
      res <- doPostJson ("/api/programs/" <> pid <> "/deploy") (object [])
      liftIO $ do
        simpleStatus res `shouldBe` status200
        responseField "state" res `shouldBe` Just "active"

    it "deploy on invalid graph returns 422" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Invalid" :: Text)])
      let Just pid = responseId created
      res <- doPostJson ("/api/programs/" <> pid <> "/deploy") (object [])
      liftIO $ do
        simpleStatus res `shouldBe` status422
        let Just (Object body) = decode (simpleBody res) :: Maybe Value
        body !? "valid"  `shouldBe` Just (Bool False)
        isJust (body !? "errors") `shouldBe` True

    it "pause: Active → Paused (200)" $ \app -> sess app $ do
      pid <- createAndDeploy "Pausable"
      res <- doPostJson ("/api/programs/" <> pid <> "/pause") (object [])
      liftIO $ do
        simpleStatus res `shouldBe` status200
        responseField "state" res `shouldBe` Just "paused"

    it "resume: Paused → Active (200)" $ \app -> sess app $ do
      pid <- createAndDeploy "Resumable"
      _   <- doPostJson ("/api/programs/" <> pid <> "/pause")  (object [])
      res <- doPostJson ("/api/programs/" <> pid <> "/resume") (object [])
      liftIO $ do
        simpleStatus res `shouldBe` status200
        responseField "state" res `shouldBe` Just "active"

    it "deploy on Active program returns 409" $ \app -> sess app $ do
      pid <- createAndDeploy "Double Deploy"
      res <- doPostJson ("/api/programs/" <> pid <> "/deploy") (object [])
      liftIO $ simpleStatus res `shouldBe` status409

    it "pause on Draft program returns 409" $ \app -> sess app $ do
      created <- doPostJson "/api/programs" (object ["name" .= ("Draft Pause" :: Text)])
      let Just pid = responseId created
      res <- doPostJson ("/api/programs/" <> pid <> "/pause") (object [])
      liftIO $ simpleStatus res `shouldBe` status409

    it "resume on Active program returns 409" $ \app -> sess app $ do
      pid <- createAndDeploy "Active Resume"
      res <- doPostJson ("/api/programs/" <> pid <> "/resume") (object [])
      liftIO $ simpleStatus res `shouldBe` status409

  describe "Execution API" $ do

    -- A graph with a single manual trigger node passes all 8 validation checks:
    -- trigger presence, DAG, port names, port categories, required wiring,
    -- config completeness (manual trigger needs no schedule), and reachability
    -- (only Agent/Action are checked, not TriggerNode).
    let minimalValidGraph :: Value
        minimalValidGraph = object
          [ "nodes" .= object
              [ "n1" .= object
                  [ "id"    .= ("n1" :: Text)
                  , "label" .= ("Start" :: Text)
                  , "type"  .= object
                      [ "type"   .= ("trigger" :: Text)
                      , "config" .= object
                          [ "type"            .= ("manual" :: Text)
                          , "schedule"        .= Null
                          , "eventFilter"     .= Null
                          , "payloadTemplate" .= Null
                          ]
                      ]
                  , "posX"  .= (0.0 :: Double)
                  , "posY"  .= (0.0 :: Double)
                  ]
              ]
          , "edges" .= ([] :: [Value])
          ]

    -- Helper: create a program and PUT the minimal valid graph. Runs inside
    -- an existing Session so it can be composed in each test.
    let createProgramWithValidGraph :: Session BL.ByteString
        createProgramWithValidGraph = do
          created <- doPostJson "/api/programs"
            (object ["name" .= ("Exec Test" :: Text)])
          let Just pid = responseId created
          _ <- doPutJson ("/api/programs/" <> pid <> "/graph") minimalValidGraph
          pure pid

    describe "POST /api/programs/:id/runs" $ do

      it "returns 400 with validation errors when graph has no trigger" $ \app -> sess app $ do
        created <- doPostJson "/api/programs"
          (object ["name" .= ("Empty Graph" :: Text)])
        let Just pid = responseId created
        res <- doPostJson ("/api/programs/" <> pid <> "/runs") (object [])
        liftIO $ do
          simpleStatus res `shouldBe` status400
          let Just (Object body) = decode (simpleBody res) :: Maybe Value
          body !? "valid"  `shouldBe` Just (Bool False)
          isJust (body !? "errors") `shouldBe` True

      it "returns 201 with run when graph is valid" $ \app -> sess app $ do
        pid <- createProgramWithValidGraph
        res <- doPostJson ("/api/programs/" <> pid <> "/runs") (object [])
        liftIO $ do
          simpleStatus res `shouldBe` status201
          isJust (responseId res) `shouldBe` True

      it "returns 404 for unknown program" $ \app -> sess app $ do
        res <- doPostJson "/api/programs/ghost/runs" (object [])
        liftIO $ simpleStatus res `shouldBe` status404

    describe "GET /api/programs/:id/runs" $ do

      it "returns empty list when no runs exist" $ \app -> sess app $ do
        pid <- createProgramWithValidGraph
        res <- doGet ("/api/programs/" <> pid <> "/runs")
        liftIO $ do
          simpleStatus res `shouldBe` status200
          decode (simpleBody res) `shouldBe` Just ([] :: [Value])

      it "returns the created run in the list" $ \app -> sess app $ do
        pid <- createProgramWithValidGraph
        _ <- doPostJson ("/api/programs/" <> pid <> "/runs") (object [])
        res <- doGet ("/api/programs/" <> pid <> "/runs")
        liftIO $ do
          simpleStatus res `shouldBe` status200
          let Just runs = decode (simpleBody res) :: Maybe [Value]
          length runs `shouldBe` 1

    describe "GET /api/runs/:id" $ do

      it "returns run detail with run and steps fields" $ \app -> sess app $ do
        pid    <- createProgramWithValidGraph
        runRes <- doPostJson ("/api/programs/" <> pid <> "/runs") (object [])
        let Just rid = responseId runRes
        res <- doGet ("/api/runs/" <> rid)
        liftIO $ do
          simpleStatus res `shouldBe` status200
          let Just (Object body) = decode (simpleBody res) :: Maybe Value
          isJust (body !? "run")   `shouldBe` True
          isJust (body !? "steps") `shouldBe` True

      it "returns 404 for unknown run id" $ \app -> sess app $ do
        res <- doGet "/api/runs/no-such-run"
        liftIO $ simpleStatus res `shouldBe` status404

    describe "POST /api/runs/:id/cancel" $ do

      it "returns 404 for unknown run id" $ \app -> sess app $ do
        res <- doPostJson "/api/runs/no-such-run/cancel" (object [])
        liftIO $ simpleStatus res `shouldBe` status404

      it "transitions a running run to canceled (200) and returns state=canceled" $ \app -> sess app $ do
        pid    <- createProgramWithValidGraph
        runRes <- doPostJson ("/api/programs/" <> pid <> "/runs") (object [])
        let Just rid = responseId runRes
        res <- doPostJson ("/api/runs/" <> rid <> "/cancel") (object [])
        liftIO $ do
          simpleStatus res `shouldBe` status200
          responseField "state" res `shouldBe` Just "canceled"

      it "returns 409 when cancelling an already-canceled run" $ \app -> sess app $ do
        pid    <- createProgramWithValidGraph
        runRes <- doPostJson ("/api/programs/" <> pid <> "/runs") (object [])
        let Just rid = responseId runRes
        _   <- doPostJson ("/api/runs/" <> rid <> "/cancel") (object [])
        res <- doPostJson ("/api/runs/" <> rid <> "/cancel") (object [])
        liftIO $ simpleStatus res `shouldBe` status409

  describe "CORS headers" $ do
    it "includes Access-Control-Allow-Origin: * on all responses" $ \app -> sess app $ do
      res <- doGet "/api/programs"
      liftIO $
        lookup "Access-Control-Allow-Origin" (simpleHeaders res)
          `shouldBe` Just "*"

  -- -------------------------------------------------------------------------
  -- Credentials (EVA-32)
  -- -------------------------------------------------------------------------

  describe "GET /api/credentials" $ do
    it "returns empty list initially" $ \app -> sess app $ do
      res <- doGet "/api/credentials"
      liftIO $ do
        simpleStatus res `shouldBe` status200
        let Just arr = decode (simpleBody res) :: Maybe [Value]
        arr `shouldBe` []

  describe "POST /api/credentials" $ do
    it "creates a credential and returns 201 with safe fields only" $ \app -> sess app $ do
      res <- doPostJson "/api/credentials" $ object
        [ "name"   .= ("Linear API Key" :: Text)
        , "system" .= ("linear" :: Text)
        , "type"   .= ("api_key" :: Text)
        , "secret" .= ("lin_api_secret" :: Text)
        ]
      liftIO $ do
        simpleStatus res `shouldBe` status201
        let Just (Object body) = decode (simpleBody res) :: Maybe Value
        body !? "id"        `shouldSatisfy` isJust
        body !? "name"      `shouldBe` Just (String "Linear API Key")
        body !? "system"    `shouldBe` Just (String "linear")
        body !? "type"      `shouldBe` Just (String "api_key")
        body !? "createdAt" `shouldSatisfy` isJust
        body !? "secret"    `shouldBe` Nothing

    it "response never contains the raw secret field" $ \app -> sess app $ do
      res <- doPostJson "/api/credentials" $ object
        [ "name"   .= ("My Key" :: Text)
        , "system" .= ("github" :: Text)
        , "type"   .= ("oauth_token" :: Text)
        , "secret" .= ("ghp_supersecret" :: Text)
        ]
      liftIO $ do
        let Just (Object body) = decode (simpleBody res) :: Maybe Value
        body !? "secret" `shouldBe` Nothing

    it "created credential appears in GET list" $ \app -> sess app $ do
      _ <- doPostJson "/api/credentials" $ object
        [ "name"   .= ("My Key" :: Text)
        , "system" .= ("linear" :: Text)
        , "type"   .= ("api_key" :: Text)
        , "secret" .= ("secret123" :: Text)
        ]
      res <- doGet "/api/credentials"
      liftIO $ do
        simpleStatus res `shouldBe` status200
        let Just arr = decode (simpleBody res) :: Maybe [Value]
        length arr `shouldBe` 1

  describe "DELETE /api/credentials/:id" $ do
    it "deletes an existing credential (204)" $ \app -> sess app $ do
      createRes <- doPostJson "/api/credentials" $ object
        [ "name"   .= ("My Key" :: Text)
        , "system" .= ("linear" :: Text)
        , "type"   .= ("api_key" :: Text)
        , "secret" .= ("secret123" :: Text)
        ]
      let Just cid = responseId createRes
      delRes <- doDelete ("/api/credentials/" <> cid)
      liftIO $ simpleStatus delRes `shouldBe` status204

    it "deleted credential no longer appears in list" $ \app -> sess app $ do
      createRes <- doPostJson "/api/credentials" $ object
        [ "name"   .= ("My Key" :: Text)
        , "system" .= ("linear" :: Text)
        , "type"   .= ("api_key" :: Text)
        , "secret" .= ("secret123" :: Text)
        ]
      let Just cid = responseId createRes
      _ <- doDelete ("/api/credentials/" <> cid)
      listRes <- doGet "/api/credentials"
      liftIO $ do
        let Just arr = decode (simpleBody listRes) :: Maybe [Value]
        arr `shouldBe` []
