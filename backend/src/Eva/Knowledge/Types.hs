{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the Auto-Knowledge feature (P2-M5).
-- Types-only module — no persistence entities (those live in Eva.Persistence.Schema),
-- no extraction logic (EVA-77 Extract.Codebase, EVA-78 Extract.Linear),
-- no query logic (EVA-79 Knowledge.Query).
module Eva.Knowledge.Types
  ( -- * Identifiers
    KnowledgeEntryId (..)

    -- * Enumerations
  , KnowledgeSourceType (..)
  , KnowledgeCategory (..)

    -- * Domain types
  , KnowledgeEntry (..)
  , SearchQuery (..)
  , SearchResult (..)
  ) where

import Data.Aeson
import Data.Char (toLower)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Eva.Core.Types (ProgramId)

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

newtype KnowledgeEntryId = KnowledgeEntryId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

-- ---------------------------------------------------------------------------
-- Source type
-- ---------------------------------------------------------------------------

-- | Where a KnowledgeEntry was extracted from.
-- Serializes to lowercase after stripping the "Source" prefix:
-- SourceCodebase -> "codebase", SourceLinear -> "linear", etc.
data KnowledgeSourceType
  = SourceCodebase
  | SourceLinear
  | SourceGitHub
  | SourceHttp
  | SourceManual
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

knowledgeSourceTypeOptions :: Options
knowledgeSourceTypeOptions =
  defaultOptions{constructorTagModifier = map toLower . drop 6}

instance ToJSON KnowledgeSourceType where
  toJSON = genericToJSON knowledgeSourceTypeOptions
  toEncoding = genericToEncoding knowledgeSourceTypeOptions

instance FromJSON KnowledgeSourceType where
  parseJSON = genericParseJSON knowledgeSourceTypeOptions

-- ---------------------------------------------------------------------------
-- Category
-- ---------------------------------------------------------------------------

-- | Semantic category of a KnowledgeEntry, used for filtering and display.
-- Serializes to lowercase after stripping the "Category" prefix:
-- CategoryStructure -> "structure", CategoryMetadata -> "metadata", etc.
data KnowledgeCategory
  = CategoryStructure
  | CategoryMetadata
  | CategoryPattern
  | CategorySummary
  | CategoryReference
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

knowledgeCategoryOptions :: Options
knowledgeCategoryOptions =
  defaultOptions{constructorTagModifier = map toLower . drop 8}

instance ToJSON KnowledgeCategory where
  toJSON = genericToJSON knowledgeCategoryOptions
  toEncoding = genericToEncoding knowledgeCategoryOptions

instance FromJSON KnowledgeCategory where
  parseJSON = genericParseJSON knowledgeCategoryOptions

-- ---------------------------------------------------------------------------
-- KnowledgeEntry
-- ---------------------------------------------------------------------------

-- | A single extracted and indexed knowledge item.
-- Stored in the `knowledge_entries` SQLite table; full-text indexed via
-- the `knowledge_fts` FTS5 virtual table (title + content columns).
--
-- `originalContent` stores the auto-generated content before any user edit,
-- enabling a "reset to auto-generated" operation without re-fetching the source.
-- `omitNothingFields` ensures JSON without this key decodes as `Nothing`
-- (backward-compatible with entries that predate the field).
data KnowledgeEntry = KnowledgeEntry
  { knowledgeEntryId :: KnowledgeEntryId
  , knowledgeEntrySourceType :: KnowledgeSourceType
  , knowledgeEntrySourceId :: Maybe Text
  , knowledgeEntryProgramId :: Maybe ProgramId
  , knowledgeEntryCategory :: KnowledgeCategory
  , knowledgeEntryTitle :: Text
  , knowledgeEntryContent :: Text
  , knowledgeEntryOriginalContent :: Maybe Text
  , knowledgeEntryMetadata :: Value
  , knowledgeEntryConfidence :: Double
  , knowledgeEntryIsEdited :: Bool
  , knowledgeEntryCreatedAt :: UTCTime
  , knowledgeEntryUpdatedAt :: UTCTime
  , knowledgeEntryScannedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON KnowledgeEntry where
  toJSON = genericToJSON (dropPrefix "knowledgeEntry")
  toEncoding = genericToEncoding (dropPrefix "knowledgeEntry")

instance FromJSON KnowledgeEntry where
  parseJSON = genericParseJSON (dropPrefix "knowledgeEntry")

-- ---------------------------------------------------------------------------
-- SearchQuery
-- ---------------------------------------------------------------------------

-- | Parameters for a full-text knowledge search (used by Eva.Knowledge.Query).
data SearchQuery = SearchQuery
  { searchQueryText :: Text
  , searchQuerySourceType :: Maybe KnowledgeSourceType
  , searchQueryCategory :: Maybe KnowledgeCategory
  , searchQueryProgramId :: Maybe ProgramId
  , searchQueryLimit :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SearchQuery where
  toJSON = genericToJSON (dropPrefix "searchQuery")
  toEncoding = genericToEncoding (dropPrefix "searchQuery")

instance FromJSON SearchQuery where
  parseJSON = genericParseJSON (dropPrefix "searchQuery")

-- ---------------------------------------------------------------------------
-- SearchResult
-- ---------------------------------------------------------------------------

-- | A single result from a knowledge search, paired with its relevance score.
data SearchResult = SearchResult
  { searchResultEntry :: KnowledgeEntry
  , searchResultScore :: Double
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SearchResult where
  toJSON = genericToJSON (dropPrefix "searchResult")
  toEncoding = genericToEncoding (dropPrefix "searchResult")

instance FromJSON SearchResult where
  parseJSON = genericParseJSON (dropPrefix "searchResult")
