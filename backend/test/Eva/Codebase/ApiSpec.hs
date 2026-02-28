{-# LANGUAGE OverloadedStrings #-}

module Eva.Codebase.ApiSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.KeyMap ((!?))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
  ( hContentType
  , methodDelete
  , methodGet
  , methodPost
  , methodPut
  , status200
  , status201
  , status204
  , status400
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
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
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

-- ---------------------------------------------------------------------------
-- Response helpers
-- ---------------------------------------------------------------------------

extractId :: SResponse -> Maybe BL.ByteString
extractId res = do
  Object obj <- decode (simpleBody res)
  String t   <- obj !? "id"
  pure (BL.fromStrict (TE.encodeUtf8 t))

-- | Create a program and return its ID as a URL-safe ByteString.
createProgram :: Application -> IO BL.ByteString
createProgram app = do
  res <- sess app $ doPostJson "/api/programs" (object ["name" .= ("Test Program" :: Text)])
  case decode (simpleBody res) of
    Just (Object obj) | Just (String t) <- obj !? "id" ->
      pure (BL.fromStrict (TE.encodeUtf8 t))
    _ -> fail "createProgram: unexpected response"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = before makeTestApp $ do

  describe "GET /api/programs/:id/codebase" $ do
    it "AC1: returns empty list before any codebase is connected" $ \app -> do
      pid <- createProgram app
      res <- sess app $ doGet ("/api/programs/" <> pid <> "/codebase")
      simpleStatus res `shouldBe` status200
      case decode (simpleBody res) of
        Just (Array _) -> pure ()
        _              -> expectationFailure "expected a JSON array"

  describe "POST /api/programs/:id/codebase" $ do
    it "AC2: with a valid path returns 201 and CodebaseMetadata" $ \app ->
      withSystemTempDirectory "eva-codebase-test" $ \dir -> do
        -- Write a small file so the scanner finds something
        writeFile (dir </> "main.hs") "main :: IO ()\nmain = putStrLn \"hello\"\n"
        pid <- createProgram app
        res <- sess app $ doPostJson
          ("/api/programs/" <> pid <> "/codebase")
          (object ["path" .= dir])
        simpleStatus res `shouldBe` status201
        isJust (extractId res) `shouldBe` True

    it "AC3: with path '../../../etc' returns 400 Bad Request" $ \app -> do
      pid <- createProgram app
      res <- sess app $ doPostJson
        ("/api/programs/" <> pid <> "/codebase")
        (object ["path" .= ("../../../etc" :: String)])
      simpleStatus res `shouldBe` status400

  describe "GET /api/codebase/:cbId/tree (smoke)" $ do
    it "returns 200 and a FileNode tree after connecting a codebase" $ \app ->
      withSystemTempDirectory "eva-codebase-tree" $ \dir -> do
        writeFile (dir </> "README.md") "# test\n"
        pid <- createProgram app
        -- Connect
        connRes <- sess app $ doPostJson
          ("/api/programs/" <> pid <> "/codebase")
          (object ["path" .= dir])
        simpleStatus connRes `shouldBe` status201
        cbId <- maybe (fail "expected codebase id in response") pure (extractId connRes)
        -- Tree
        treeRes <- sess app $ doGet ("/api/codebase/" <> cbId <> "/tree")
        simpleStatus treeRes `shouldBe` status200

  describe "GET /api/codebase/:cbId/diff (smoke)" $ do
    it "returns 200 after connecting a codebase" $ \app ->
      withSystemTempDirectory "eva-codebase-diff" $ \dir -> do
        pid <- createProgram app
        connRes <- sess app $ doPostJson
          ("/api/programs/" <> pid <> "/codebase")
          (object ["path" .= dir])
        simpleStatus connRes `shouldBe` status201
        cbId <- maybe (fail "expected codebase id") pure (extractId connRes)
        diffRes <- sess app $ doGet ("/api/codebase/" <> cbId <> "/diff")
        simpleStatus diffRes `shouldBe` status200

  describe "POST /api/codebase/:cbId/refresh (smoke)" $ do
    it "returns 200 with updated metadata after connecting a codebase" $ \app ->
      withSystemTempDirectory "eva-codebase-refresh" $ \dir -> do
        pid <- createProgram app
        connRes <- sess app $ doPostJson
          ("/api/programs/" <> pid <> "/codebase")
          (object ["path" .= dir])
        simpleStatus connRes `shouldBe` status201
        cbId <- maybe (fail "expected codebase id") pure (extractId connRes)
        refreshRes <- sess app $ doPostJson ("/api/codebase/" <> cbId <> "/refresh") (object [])
        simpleStatus refreshRes `shouldBe` status200

  describe "DELETE /api/programs/:id/codebase/:cbId" $ do
    it "returns 204 after disconnecting a connected codebase" $ \app ->
      withSystemTempDirectory "eva-codebase-delete" $ \dir -> do
        pid <- createProgram app
        connRes <- sess app $ doPostJson
          ("/api/programs/" <> pid <> "/codebase")
          (object ["path" .= dir])
        cbId <- maybe (fail "expected codebase id") pure (extractId connRes)
        delRes <- sess app $ doDelete ("/api/programs/" <> pid <> "/codebase/" <> cbId)
        simpleStatus delRes `shouldBe` status204

  describe "GET /api/programs/:id/changesets" $ do
    it "returns empty list before any changesets exist" $ \app -> do
      pid <- createProgram app
      res <- sess app $ doGet ("/api/programs/" <> pid <> "/changesets")
      simpleStatus res `shouldBe` status200
      case decode (simpleBody res) of
        Just (Array _) -> pure ()
        _              -> expectationFailure "expected a JSON array"
