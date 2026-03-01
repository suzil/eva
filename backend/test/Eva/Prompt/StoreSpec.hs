{-# LANGUAGE OverloadedStrings #-}

module Eva.Prompt.StoreSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import qualified Data.Text as T

import Eva.App (AppEnv (..), AppM, runAppM)
import Eva.Config (AppConfig (..), LogLevel (..))
import qualified Eva.Crypto as Crypto
import Eva.Engine.Dispatch (execute)
import Eva.Engine.LLM (dummyLLMClient)
import Eva.Persistence.Migration (runMigrations)
import Eva.Prompt.Store
import Eva.Prompt.Types

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

withTestEnv :: (AppEnv -> IO ()) -> IO ()
withTestEnv action = do
  pool             <- runNoLoggingT $ createSqlitePool ":memory:" 1
  runMigrations pool
  broadcasts       <- newTVarIO Map.empty
  cancelTokens <- newTVarIO Map.empty
  assistBroadcasts <- newTVarIO Map.empty
  let cfg = AppConfig
        { configDbPath          = ":memory:"
        , configPort            = 8080
        , configLlmApiKey       = Nothing
        , configAnthropicApiKey = Nothing
        , configLogLevel        = LogError
        , configCredentialKey   = "test-key"
        , configStaticDir       = Nothing
        }
      env = AppEnv
        { envConfig              = cfg
        , envDbPool              = pool
        , envLogger              = \_ -> pure ()
        , envDispatch            = execute
        , envLLMClient           = dummyLLMClient
        , envAnthropicClient     = dummyLLMClient
        , envBroadcasts          = broadcasts
        , envAssistantBroadcasts = assistBroadcasts
        , envCredentialKey       = Crypto.deriveKey "test-key"
        , envCancelTokens    = cancelTokens
        }
  action env

runTest :: AppEnv -> AppM a -> IO a
runTest = runAppM

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

t0 :: UTCTime
t0 = posixSecondsToUTCTime 1_740_000_000

t1 :: UTCTime
t1 = posixSecondsToUTCTime 1_740_001_000

userTemplate :: PromptTemplate
userTemplate = PromptTemplate
  { promptTemplateId          = "tmpl-user-1"
  , promptTemplateName        = "My Custom Prompt"
  , promptTemplateDescription = "A user-defined template"
  , promptTemplateCategory    = Custom
  , promptTemplateTags        = ["custom"]
  , promptTemplateBody        = "Do something with {{input}}"
  , promptTemplateVariables   =
      [ TemplateVariable "input" "The input" True Nothing ]
  , promptTemplateBuiltIn     = False
  , promptTemplateCreatedAt   = t0
  , promptTemplateUpdatedAt   = t0
  }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = around withTestEnv $ do

  describe "seedBuiltinTemplates" $ do
    it "seeds exactly 7 templates on a fresh database" $ \env -> do
      seedBuiltinTemplates (envDbPool env)
      templates <- runTest env listTemplates
      length templates `shouldBe` 7

    it "is idempotent — seeding twice still yields exactly 7 templates" $ \env -> do
      seedBuiltinTemplates (envDbPool env)
      seedBuiltinTemplates (envDbPool env)
      templates <- runTest env listTemplates
      length templates `shouldBe` 7

    it "seeds the Summarizer template with correct name and category" $ \env -> do
      seedBuiltinTemplates (envDbPool env)
      result <- runTest env (getTemplate "tmpl-summarizer")
      fmap promptTemplateName     result `shouldBe` Just "Summarizer"
      fmap promptTemplateCategory result `shouldBe` Just Summarizer
      fmap promptTemplateBuiltIn  result `shouldBe` Just True

    it "seeds all 7 expected IDs" $ \env -> do
      seedBuiltinTemplates (envDbPool env)
      templates <- runTest env listTemplates
      let ids = map ((\(TemplateId t) -> t) . promptTemplateId) templates
      ids `shouldContain` ["tmpl-summarizer"]
      ids `shouldContain` ["tmpl-code-reviewer"]
      ids `shouldContain` ["tmpl-issue-classifier"]
      ids `shouldContain` ["tmpl-data-extractor"]
      ids `shouldContain` ["tmpl-report-formatter"]
      ids `shouldContain` ["tmpl-meeting-notes-analyst"]
      ids `shouldContain` ["tmpl-custom"]

  describe "insertTemplate / getTemplate" $ do
    it "round-trips a user PromptTemplate through the database" $ \env -> do
      runTest env (insertTemplate userTemplate)
      result <- runTest env (getTemplate "tmpl-user-1")
      result `shouldBe` Just userTemplate

    it "getTemplate returns Nothing for an unknown id" $ \env -> do
      result <- runTest env (getTemplate "no-such-template")
      result `shouldBe` Nothing

  describe "updateTemplate" $ do
    it "persists body and variable changes" $ \env -> do
      runTest env (insertTemplate userTemplate)
      let updated = userTemplate
            { promptTemplateBody        = "Updated body {{input}}"
            , promptTemplateDescription = "Updated description"
            , promptTemplateUpdatedAt   = t1
            }
      runTest env (updateTemplate updated)
      result <- runTest env (getTemplate "tmpl-user-1")
      fmap promptTemplateBody        result `shouldBe` Just "Updated body {{input}}"
      fmap promptTemplateDescription result `shouldBe` Just "Updated description"

    it "does not modify the builtIn flag when updating a built-in template" $ \env -> do
      seedBuiltinTemplates (envDbPool env)
      mTmpl <- runTest env (getTemplate "tmpl-summarizer")
      case mTmpl of
        Nothing   -> expectationFailure "tmpl-summarizer not found after seeding"
        Just tmpl -> do
          let modified = tmpl { promptTemplateBody = "New body {{content}}", promptTemplateUpdatedAt = t1 }
          runTest env (updateTemplate modified)
          result <- runTest env (getTemplate "tmpl-summarizer")
          fmap promptTemplateBody    result `shouldBe` Just "New body {{content}}"
          fmap promptTemplateBuiltIn result `shouldBe` Just True

  describe "deleteTemplate" $ do
    it "removes a user-created template — returns Right ()" $ \env -> do
      runTest env (insertTemplate userTemplate)
      result <- runTest env (deleteTemplate "tmpl-user-1")
      result `shouldBe` Right ()
      gone <- runTest env (getTemplate "tmpl-user-1")
      gone `shouldBe` Nothing

    it "rejects deletion of a built-in template — returns Left" $ \env -> do
      seedBuiltinTemplates (envDbPool env)
      result <- runTest env (deleteTemplate "tmpl-summarizer")
      case result of
        Left msg -> T.isInfixOf "built-in" msg `shouldBe` True
        Right () -> expectationFailure "expected Left for built-in template"

    it "returns Left for a non-existent template" $ \env -> do
      result <- runTest env (deleteTemplate "no-such-id")
      case result of
        Left _  -> pure ()
        Right () -> expectationFailure "expected Left for missing template"

  describe "listTemplates" $ do
    it "returns an empty list on a fresh database" $ \env -> do
      templates <- runTest env listTemplates
      templates `shouldBe` []

    it "returns all inserted templates in createdAt order" $ \env -> do
      runTest env (insertTemplate userTemplate)
      let t2 = userTemplate
            { promptTemplateId        = "tmpl-user-2"
            , promptTemplateName      = "Second Template"
            , promptTemplateCreatedAt = t1
            , promptTemplateUpdatedAt = t1
            }
      runTest env (insertTemplate t2)
      templates <- runTest env listTemplates
      map promptTemplateName templates `shouldBe` ["My Custom Prompt", "Second Template"]
