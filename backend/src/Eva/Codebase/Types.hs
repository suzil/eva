{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the Codebase Integration feature (P2-M4).
-- Types-only module — no persistence entities (those live in EVA-67),
-- no handlers (EVA-66 scanner, EVA-68 API, EVA-69 connector handler).
module Eva.Codebase.Types
  ( -- * Identifiers
    CodebaseId (..)
  , CodeChangesetId (..)
  , FileChangeId (..)

    -- * Language statistics
  , LangStats

    -- * File tree
  , FileNode (..)
  , FileEntry (..)

    -- * Codebase metadata
  , CodebaseMetadata (..)

    -- * Changeset types
  , CodeChangesetStatus (..)
  , FileChangeAction (..)
  , FileChangeStatus (..)
  , CodeChangeset (..)
  , FileChange (..)
  ) where

import Data.Aeson
import Data.Char (toLower)
import Data.Map.Strict (Map)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Eva.Core.Types (ProgramId, RunId, StepId)

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

newtype CodebaseId = CodebaseId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype CodeChangesetId = CodeChangesetId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype FileChangeId = FileChangeId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

-- ---------------------------------------------------------------------------
-- Language statistics
-- ---------------------------------------------------------------------------

-- | Maps file extension (e.g. "hs", "ts") to file count.
type LangStats = Map Text Int

-- ---------------------------------------------------------------------------
-- File tree
-- ---------------------------------------------------------------------------

-- | A node in a directory tree. Children is empty for plain files.
data FileNode = FileNode
  { fileNodeName :: Text
  , fileNodePath :: Text
  , fileNodeIsDir :: Bool
  , fileNodeChildren :: [FileNode]
  , fileNodeSize :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FileNode where
  toJSON = genericToJSON (dropPrefix "fileNode")
  toEncoding = genericToEncoding (dropPrefix "fileNode")

instance FromJSON FileNode where
  parseJSON = genericParseJSON (dropPrefix "fileNode")

-- | A single file's path and full content, returned by the scanner.
data FileEntry = FileEntry
  { fileEntryPath :: Text
  , fileEntryLanguage :: Text
  , fileEntryContent :: Text
  , fileEntrySize :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FileEntry where
  toJSON = genericToJSON (dropPrefix "fileEntry")
  toEncoding = genericToEncoding (dropPrefix "fileEntry")

instance FromJSON FileEntry where
  parseJSON = genericParseJSON (dropPrefix "fileEntry")

-- ---------------------------------------------------------------------------
-- Codebase metadata
-- ---------------------------------------------------------------------------

-- | Snapshot of a scanned codebase root, associated with one program.
data CodebaseMetadata = CodebaseMetadata
  { codebaseMetaId :: CodebaseId
  , codebaseMetaProgramId :: ProgramId
  , codebaseMetaPath :: Text
  , codebaseMetaLanguageStats :: LangStats
  , codebaseMetaKeyFiles :: [Text]
  , codebaseMetaGitBranch :: Maybe Text
  , codebaseMetaGitDirty :: Bool
  , codebaseMetaLastScannedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CodebaseMetadata where
  toJSON = genericToJSON (dropPrefix "codebaseMeta")
  toEncoding = genericToEncoding (dropPrefix "codebaseMeta")

instance FromJSON CodebaseMetadata where
  parseJSON = genericParseJSON (dropPrefix "codebaseMeta")

-- ---------------------------------------------------------------------------
-- Changeset status
-- ---------------------------------------------------------------------------

-- | Lifecycle state of a code changeset proposed by an agent step.
-- Serializes to lowercase: "pending" | "applied" | "rejected".
data CodeChangesetStatus
  = ChangesetPending
  | ChangesetApplied
  | ChangesetRejected
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

codeChangesetStatusOptions :: Options
codeChangesetStatusOptions =
  defaultOptions{constructorTagModifier = map toLower . drop 9}

instance ToJSON CodeChangesetStatus where
  toJSON = genericToJSON codeChangesetStatusOptions
  toEncoding = genericToEncoding codeChangesetStatusOptions

instance FromJSON CodeChangesetStatus where
  parseJSON = genericParseJSON codeChangesetStatusOptions

-- ---------------------------------------------------------------------------
-- File change action
-- ---------------------------------------------------------------------------

-- | The kind of change an agent is proposing for a single file.
-- Serializes to lowercase: "add" | "modify" | "delete".
data FileChangeAction
  = FileActionAdd
  | FileActionModify
  | FileActionDelete
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

fileChangeActionOptions :: Options
fileChangeActionOptions =
  defaultOptions{constructorTagModifier = map toLower . drop 10}

instance ToJSON FileChangeAction where
  toJSON = genericToJSON fileChangeActionOptions
  toEncoding = genericToEncoding fileChangeActionOptions

instance FromJSON FileChangeAction where
  parseJSON = genericParseJSON fileChangeActionOptions

-- ---------------------------------------------------------------------------
-- File change status
-- ---------------------------------------------------------------------------

-- | Review state of an individual file change within a changeset.
-- Serializes to lowercase: "pending" | "accepted" | "rejected".
data FileChangeStatus
  = FileChangePending
  | FileChangeAccepted
  | FileChangeRejected
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

fileChangeStatusOptions :: Options
fileChangeStatusOptions =
  defaultOptions{constructorTagModifier = map toLower . drop 10}

instance ToJSON FileChangeStatus where
  toJSON = genericToJSON fileChangeStatusOptions
  toEncoding = genericToEncoding fileChangeStatusOptions

instance FromJSON FileChangeStatus where
  parseJSON = genericParseJSON fileChangeStatusOptions

-- ---------------------------------------------------------------------------
-- Code changeset
-- ---------------------------------------------------------------------------

-- | A set of file changes proposed by an agent during a run step.
-- Stored pending review; applied atomically when the user accepts.
data CodeChangeset = CodeChangeset
  { codeChangesetId :: CodeChangesetId
  , codeChangesetRunId :: RunId
  , codeChangesetStepId :: StepId
  , codeChangesetStatus :: CodeChangesetStatus
  , codeChangesetFiles :: [FileChange]
  , codeChangesetCreatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CodeChangeset where
  toJSON = genericToJSON (dropPrefix "codeChangeset")
  toEncoding = genericToEncoding (dropPrefix "codeChangeset")

instance FromJSON CodeChangeset where
  parseJSON = genericParseJSON (dropPrefix "codeChangeset")

-- ---------------------------------------------------------------------------
-- File change
-- ---------------------------------------------------------------------------

-- | A single file modification within a `CodeChangeset`.
data FileChange = FileChange
  { fileChangeId :: FileChangeId
  , fileChangeChangesetId :: CodeChangesetId
  , fileChangePath :: Text
  , fileChangeAction :: FileChangeAction
  , fileChangeOriginalContent :: Maybe Text
  , fileChangeProposedContent :: Text
  , fileChangeStatus :: FileChangeStatus
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FileChange where
  toJSON = genericToJSON (dropPrefix "fileChange")
  toEncoding = genericToEncoding (dropPrefix "fileChange")

instance FromJSON FileChange where
  parseJSON = genericParseJSON (dropPrefix "fileChange")
