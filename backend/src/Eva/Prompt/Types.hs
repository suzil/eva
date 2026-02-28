{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the Prompt Assistance feature (P2-M7).
-- Types-only module — no persistence entities (those live in Eva.Persistence.Schema),
-- no store logic (EVA-97 Prompt.Store), no resolution logic (EVA-98 Prompt.Resolve).
module Eva.Prompt.Types
  ( -- * Identifiers
    TemplateId (..)

    -- * Enumerations
  , TemplateCategory (..)

    -- * Domain types
  , TemplateVariable (..)
  , PromptTemplate (..)

    -- * Built-in template bodies
  , builtinSummarizerBody
  , builtinCodeReviewerBody
  , builtinIssueClassifierBody
  , builtinDataExtractorBody
  , builtinReportFormatterBody
  , builtinMeetingNotesAnalystBody
  , builtinCustomBody
  ) where

import Data.Aeson
import Data.Char (toLower)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- Local helper (mirrors dropPrefix in Core.Types — not exported from there)
-- ---------------------------------------------------------------------------

dropPrefix :: String -> Options
dropPrefix prefix =
  defaultOptions
    { fieldLabelModifier = lowerFirst . drop (length prefix)
    , omitNothingFields = True
    }
  where
    lowerFirst [] = []
    lowerFirst (c : cs) = toLower c : cs

-- ---------------------------------------------------------------------------
-- Identifiers
-- ---------------------------------------------------------------------------

newtype TemplateId = TemplateId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

-- ---------------------------------------------------------------------------
-- Category
-- ---------------------------------------------------------------------------

-- | Semantic category of a PromptTemplate, used for filtering and display in
-- the TemplatePicker. Serializes to lowercase constructor name directly
-- (no prefix to strip): Summarizer -> "summarizer", Custom -> "custom", etc.
data TemplateCategory
  = Summarizer
  | Reviewer
  | Classifier
  | Extractor
  | Formatter
  | Analyst
  | Custom
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

templateCategoryOptions :: Options
templateCategoryOptions =
  defaultOptions{constructorTagModifier = map toLower}

instance ToJSON TemplateCategory where
  toJSON = genericToJSON templateCategoryOptions
  toEncoding = genericToEncoding templateCategoryOptions

instance FromJSON TemplateCategory where
  parseJSON = genericParseJSON templateCategoryOptions

-- ---------------------------------------------------------------------------
-- TemplateVariable
-- ---------------------------------------------------------------------------

-- | A declared variable within a PromptTemplate body, referenced as
-- @{{name}}@ in the template body text.
-- @required = True@ with no @defaultValue@ is valid and means the user must
-- supply a binding before the template can be resolved.
-- @omitNothingFields@ ensures @defaultValue@ is absent from JSON when Nothing.
data TemplateVariable = TemplateVariable
  { templateVariableName :: Text
  , templateVariableDescription :: Text
  , templateVariableRequired :: Bool
  , templateVariableDefaultValue :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON TemplateVariable where
  toJSON = genericToJSON (dropPrefix "templateVariable")
  toEncoding = genericToEncoding (dropPrefix "templateVariable")

instance FromJSON TemplateVariable where
  parseJSON = genericParseJSON (dropPrefix "templateVariable")

-- ---------------------------------------------------------------------------
-- PromptTemplate
-- ---------------------------------------------------------------------------

-- | A reusable prompt template that can be inserted into an AgentConfig body.
-- Built-in templates (@builtIn = True@) are seeded at startup by EVA-97 and
-- are not user-deletable. @tags@ is a flat list of strings for search/filter.
-- @variables@ declares all @{{name}}@ placeholders appearing in @body@.
data PromptTemplate = PromptTemplate
  { promptTemplateId :: TemplateId
  , promptTemplateName :: Text
  , promptTemplateDescription :: Text
  , promptTemplateCategory :: TemplateCategory
  , promptTemplateTags :: [Text]
  , promptTemplateBody :: Text
  , promptTemplateVariables :: [TemplateVariable]
  , promptTemplateBuiltIn :: Bool
  , promptTemplateCreatedAt :: UTCTime
  , promptTemplateUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON PromptTemplate where
  toJSON = genericToJSON (dropPrefix "promptTemplate")
  toEncoding = genericToEncoding (dropPrefix "promptTemplate")

instance FromJSON PromptTemplate where
  parseJSON = genericParseJSON (dropPrefix "promptTemplate")

-- ---------------------------------------------------------------------------
-- Built-in template bodies
-- ---------------------------------------------------------------------------

-- | Summarize the provided content concisely, preserving key facts.
builtinSummarizerBody :: Text
builtinSummarizerBody =
  "Summarize the following content concisely, preserving all key facts and decisions.\n\n\
  \{{content}}"

-- | Review the provided code for correctness, clarity, and best practices.
builtinCodeReviewerBody :: Text
builtinCodeReviewerBody =
  "Review the following code for correctness, clarity, and adherence to best practices.\n\
  \Identify any bugs, potential improvements, or style issues. Be specific.\n\n\
  \{{code}}"

-- | Classify the provided Linear issue into a category with a brief rationale.
builtinIssueClassifierBody :: Text
builtinIssueClassifierBody =
  "Classify the following issue into one of these categories: bug, feature, chore, docs.\n\
  \Return a JSON object with fields: category (string), confidence (0-1), rationale (string).\n\n\
  \{{issue}}"

-- | Extract structured data from the provided text according to the schema.
builtinDataExtractorBody :: Text
builtinDataExtractorBody =
  "Extract structured data from the text below according to this schema:\n\
  \{{schema}}\n\n\
  \Return valid JSON matching the schema exactly.\n\n\
  \Text:\n{{text}}"

-- | Format the provided content as a structured report with sections.
builtinReportFormatterBody :: Text
builtinReportFormatterBody =
  "Format the following content as a structured report with clear sections,\n\
  \headings, and a concise executive summary at the top.\n\n\
  \{{content}}"

-- | Analyse meeting notes and extract action items with owners and due dates.
builtinMeetingNotesAnalystBody :: Text
builtinMeetingNotesAnalystBody =
  "Analyse the following meeting notes. Extract:\n\
  \1. Action items (with owner and due date if mentioned)\n\
  \2. Key decisions made\n\
  \3. Open questions\n\n\
  \{{notes}}"

-- | Blank template with a single {{input}} variable — starting point for custom prompts.
builtinCustomBody :: Text
builtinCustomBody = "{{input}}"
