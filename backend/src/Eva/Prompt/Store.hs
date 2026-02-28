{-# LANGUAGE OverloadedStrings #-}

-- | CRUD functions for prompt_templates and startup seeding of built-in templates.
-- Built-in templates (builtIn = True) cannot be deleted but their body and
-- variables can be updated. Seeding is idempotent: running seedBuiltinTemplates
-- twice results in exactly 7 templates, never duplicates.
module Eva.Prompt.Store
  ( insertTemplate
  , updateTemplate
  , deleteTemplate
  , getTemplate
  , listTemplates
  , seedBuiltinTemplates
    -- * Row conversion helpers (used by Eva.Prompt.Resolve)
  , templateFromRow
  , fromTemplateRowId
  ) where

import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist.Sql
  ( ConnectionPool
  , Entity (..)
  , SelectOpt (..)
  , delete
  , get
  , insertKey
  , runSqlPool
  , selectList
  , update
  , (=.)
  )

import Eva.App (AppM)
import Eva.Persistence.Queries (runDb)
import Eva.Persistence.Schema
import Eva.Prompt.Types

-- ---------------------------------------------------------------------------
-- Encoding helpers (mirrors Knowledge.Store — not re-exported from there)
-- ---------------------------------------------------------------------------

toJsonText :: ToJSON a => a -> Text
toJsonText = TE.decodeUtf8 . BL.toStrict . encode

fromJsonText :: FromJSON a => Text -> Either String a
fromJsonText = eitherDecodeStrict . TE.encodeUtf8

-- ---------------------------------------------------------------------------
-- ID helpers
-- ---------------------------------------------------------------------------

toTemplateRowId :: TemplateId -> PromptTemplateRowId
toTemplateRowId (TemplateId t) = PromptTemplateRowKey t

fromTemplateRowId :: PromptTemplateRowId -> TemplateId
fromTemplateRowId (PromptTemplateRowKey t) = TemplateId t

-- ---------------------------------------------------------------------------
-- Row <-> Domain
-- ---------------------------------------------------------------------------

templateToRow :: PromptTemplate -> PromptTemplateRow
templateToRow t =
  PromptTemplateRow
    { promptTemplateRowName        = promptTemplateName t
    , promptTemplateRowDescription = promptTemplateDescription t
    , promptTemplateRowCategory    = toJsonText (promptTemplateCategory t)
    , promptTemplateRowTags        = toJsonText (promptTemplateTags t)
    , promptTemplateRowBody        = promptTemplateBody t
    , promptTemplateRowVariables   = toJsonText (promptTemplateVariables t)
    , promptTemplateRowBuiltIn     = promptTemplateBuiltIn t
    , promptTemplateRowCreatedAt   = promptTemplateCreatedAt t
    , promptTemplateRowUpdatedAt   = promptTemplateUpdatedAt t
    }

templateFromRow :: PromptTemplateRowId -> PromptTemplateRow -> Either String PromptTemplate
templateFromRow rowId row = do
  cat  <- fromJsonText (promptTemplateRowCategory row)
  tags <- fromJsonText (promptTemplateRowTags row)
  vars <- fromJsonText (promptTemplateRowVariables row)
  pure PromptTemplate
    { promptTemplateId          = fromTemplateRowId rowId
    , promptTemplateName        = promptTemplateRowName row
    , promptTemplateDescription = promptTemplateRowDescription row
    , promptTemplateCategory    = cat
    , promptTemplateTags        = tags
    , promptTemplateBody        = promptTemplateRowBody row
    , promptTemplateVariables   = vars
    , promptTemplateBuiltIn     = promptTemplateRowBuiltIn row
    , promptTemplateCreatedAt   = promptTemplateRowCreatedAt row
    , promptTemplateUpdatedAt   = promptTemplateRowUpdatedAt row
    }

-- ---------------------------------------------------------------------------
-- CRUD
-- ---------------------------------------------------------------------------

insertTemplate :: PromptTemplate -> AppM ()
insertTemplate t = runDb $
  insertKey (toTemplateRowId (promptTemplateId t)) (templateToRow t)

-- | Update a template's editable fields. The builtIn flag is never modified.
updateTemplate :: PromptTemplate -> AppM ()
updateTemplate t = runDb $
  update (toTemplateRowId (promptTemplateId t))
    [ PromptTemplateRowName        =. promptTemplateName t
    , PromptTemplateRowDescription =. promptTemplateDescription t
    , PromptTemplateRowCategory    =. toJsonText (promptTemplateCategory t)
    , PromptTemplateRowTags        =. toJsonText (promptTemplateTags t)
    , PromptTemplateRowBody        =. promptTemplateBody t
    , PromptTemplateRowVariables   =. toJsonText (promptTemplateVariables t)
    , PromptTemplateRowUpdatedAt   =. promptTemplateUpdatedAt t
    ]

-- | Delete a template by ID. Returns Left if the template is built-in or
-- does not exist. EVA-100 maps the Left case to HTTP 403/404.
deleteTemplate :: TemplateId -> AppM (Either Text ())
deleteTemplate tid = do
  let rowId = toTemplateRowId tid
  mRow <- runDb $ get rowId
  case mRow of
    Nothing  -> pure (Left "template not found")
    Just row ->
      if promptTemplateRowBuiltIn row
        then pure (Left "built-in templates cannot be deleted")
        else runDb (delete rowId) >> pure (Right ())

getTemplate :: TemplateId -> AppM (Maybe PromptTemplate)
getTemplate tid = runDb $ do
  let rowId = toTemplateRowId tid
  mRow <- get rowId
  case mRow of
    Nothing  -> pure Nothing
    Just row -> case templateFromRow rowId row of
      Left err -> fail $ "getTemplate: " <> err
      Right t  -> pure (Just t)

listTemplates :: AppM [PromptTemplate]
listTemplates = runDb $ do
  entities <- selectList [] [Asc PromptTemplateRowCreatedAt]
  traverse decode entities
  where
    decode (Entity k row) = case templateFromRow k row of
      Left err -> fail $ "listTemplates: " <> err
      Right t  -> pure t

-- ---------------------------------------------------------------------------
-- Startup seeding
-- ---------------------------------------------------------------------------

-- | Idempotently seed the 7 built-in prompt templates on startup.
-- Uses get-then-insertKey per template: if a row with the stable ID already
-- exists (restart scenario), it is left unchanged. Safe to call on every
-- application startup.
seedBuiltinTemplates :: ConnectionPool -> IO ()
seedBuiltinTemplates pool = do
  now <- getCurrentTime
  runNoLoggingT $ runSqlPool (mapM_ seedOne (builtins now)) pool
  where
    seedOne tmpl = do
      let rowId = toTemplateRowId (promptTemplateId tmpl)
      existing <- get rowId
      case existing of
        Just _  -> pure ()
        Nothing -> insertKey rowId (templateToRow tmpl)

    builtins :: UTCTime -> [PromptTemplate]
    builtins now =
      [ PromptTemplate
          { promptTemplateId          = "tmpl-summarizer"
          , promptTemplateName        = "Summarizer"
          , promptTemplateDescription = "Summarize content concisely, preserving all key facts."
          , promptTemplateCategory    = Summarizer
          , promptTemplateTags        = ["summary", "text", "content"]
          , promptTemplateBody        = builtinSummarizerBody
          , promptTemplateVariables   =
              [ TemplateVariable "content" "Content to summarize" True Nothing ]
          , promptTemplateBuiltIn     = True
          , promptTemplateCreatedAt   = now
          , promptTemplateUpdatedAt   = now
          }
      , PromptTemplate
          { promptTemplateId          = "tmpl-code-reviewer"
          , promptTemplateName        = "Code Reviewer"
          , promptTemplateDescription = "Review code for correctness, clarity, and best practices."
          , promptTemplateCategory    = Reviewer
          , promptTemplateTags        = ["code", "review", "quality"]
          , promptTemplateBody        = builtinCodeReviewerBody
          , promptTemplateVariables   =
              [ TemplateVariable "code" "Code to review" True Nothing ]
          , promptTemplateBuiltIn     = True
          , promptTemplateCreatedAt   = now
          , promptTemplateUpdatedAt   = now
          }
      , PromptTemplate
          { promptTemplateId          = "tmpl-issue-classifier"
          , promptTemplateName        = "Issue Classifier"
          , promptTemplateDescription = "Classify a Linear issue into bug, feature, chore, or docs."
          , promptTemplateCategory    = Classifier
          , promptTemplateTags        = ["linear", "issue", "classification"]
          , promptTemplateBody        = builtinIssueClassifierBody
          , promptTemplateVariables   =
              [ TemplateVariable "issue" "Issue title and description" True Nothing ]
          , promptTemplateBuiltIn     = True
          , promptTemplateCreatedAt   = now
          , promptTemplateUpdatedAt   = now
          }
      , PromptTemplate
          { promptTemplateId          = "tmpl-data-extractor"
          , promptTemplateName        = "Data Extractor"
          , promptTemplateDescription = "Extract structured data from text according to a JSON schema."
          , promptTemplateCategory    = Extractor
          , promptTemplateTags        = ["extraction", "json", "structured"]
          , promptTemplateBody        = builtinDataExtractorBody
          , promptTemplateVariables   =
              [ TemplateVariable "schema" "JSON schema to extract into" True Nothing
              , TemplateVariable "text" "Text to extract data from" True Nothing
              ]
          , promptTemplateBuiltIn     = True
          , promptTemplateCreatedAt   = now
          , promptTemplateUpdatedAt   = now
          }
      , PromptTemplate
          { promptTemplateId          = "tmpl-report-formatter"
          , promptTemplateName        = "Report Formatter"
          , promptTemplateDescription = "Format content as a structured report with headings and an executive summary."
          , promptTemplateCategory    = Formatter
          , promptTemplateTags        = ["report", "formatting", "structure"]
          , promptTemplateBody        = builtinReportFormatterBody
          , promptTemplateVariables   =
              [ TemplateVariable "content" "Content to format as a report" True Nothing ]
          , promptTemplateBuiltIn     = True
          , promptTemplateCreatedAt   = now
          , promptTemplateUpdatedAt   = now
          }
      , PromptTemplate
          { promptTemplateId          = "tmpl-meeting-notes-analyst"
          , promptTemplateName        = "Meeting Notes Analyst"
          , promptTemplateDescription = "Extract action items, key decisions, and open questions from meeting notes."
          , promptTemplateCategory    = Analyst
          , promptTemplateTags        = ["meeting", "notes", "action-items"]
          , promptTemplateBody        = builtinMeetingNotesAnalystBody
          , promptTemplateVariables   =
              [ TemplateVariable "notes" "Meeting notes to analyse" True Nothing ]
          , promptTemplateBuiltIn     = True
          , promptTemplateCreatedAt   = now
          , promptTemplateUpdatedAt   = now
          }
      , PromptTemplate
          { promptTemplateId          = "tmpl-custom"
          , promptTemplateName        = "Custom"
          , promptTemplateDescription = "Blank template with a single {{input}} variable — starting point for custom prompts."
          , promptTemplateCategory    = Custom
          , promptTemplateTags        = ["custom", "blank"]
          , promptTemplateBody        = builtinCustomBody
          , promptTemplateVariables   =
              [ TemplateVariable "input" "Custom input" False Nothing ]
          , promptTemplateBuiltIn     = True
          , promptTemplateCreatedAt   = now
          , promptTemplateUpdatedAt   = now
          }
      ]
