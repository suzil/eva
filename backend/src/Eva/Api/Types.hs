{-# LANGUAGE OverloadedStrings #-}

-- | API request/response JSON types (distinct from domain types).
-- Domain types (Program, Graph, Node, Edge) are used directly as response
-- bodies where possible; these types cover request bodies and error shapes.
module Eva.Api.Types
  ( -- * Request bodies
    CreateProgramReq (..)
  , PatchProgramReq (..)
  , CreateRunReq (..)
  , CreateCredentialReq (..)
  , SpecRequest (..)
  , ConnectCodebaseReq (..)
  , WriteFileReq (..)

    -- * Response bodies
  , HealthResponse (..)
  , ValidateResult (..)
  , RunDetail (..)
  , SpecResponse (..)
  , GitDiffFile (..)
  , GitDiffResponse (..)

    -- * Error response
  , ApiError (..)
  , SpecError (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)

import Eva.Core.Types (Run, Step, ValidationError (..), CredentialType (..), SystemType (..))
import Eva.Declarative (ParseError)

-- ---------------------------------------------------------------------------
-- Request bodies
-- ---------------------------------------------------------------------------

newtype CreateProgramReq = CreateProgramReq
  { cprName :: Text
  }

instance FromJSON CreateProgramReq where
  parseJSON = withObject "CreateProgramReq" $ \o ->
    CreateProgramReq <$> o .: "name"

-- | Only 'name' is patchable at M1. State changes go through transition
-- endpoints (deploy/pause/resume). Graph changes go through PUT .../graph.
newtype PatchProgramReq = PatchProgramReq
  { pprName :: Maybe Text
  }

instance FromJSON PatchProgramReq where
  parseJSON = withObject "PatchProgramReq" $ \o ->
    PatchProgramReq <$> o .:? "name"

-- ---------------------------------------------------------------------------
-- Response bodies
-- ---------------------------------------------------------------------------

newtype HealthResponse = HealthResponse
  { hrStatus :: Text
  }

instance ToJSON HealthResponse where
  toJSON r = object ["status" .= hrStatus r]

data ValidateResult = ValidateResult
  { vrValid  :: Bool
  , vrErrors :: [ValidationError]
  }

instance ToJSON ValidateResult where
  toJSON r = object ["valid" .= vrValid r, "errors" .= vrErrors r]

-- | Request body for POST /api/programs/:id/runs.
-- 'triggerPayload' is forwarded to the Trigger node as its seed value.
newtype CreateRunReq = CreateRunReq
  { crrTriggerPayload :: Maybe Value
  }

instance FromJSON CreateRunReq where
  parseJSON = withObject "CreateRunReq" $ \o ->
    CreateRunReq <$> o .:? "triggerPayload"

-- | Request body for POST /api/credentials.
-- 'secret' is the raw credential value (API key, token, etc.) — encrypted before storage.
data CreateCredentialReq = CreateCredentialReq
  { ccrName   :: Text
  , ccrSystem :: SystemType
  , ccrType   :: CredentialType
  , ccrSecret :: Text
  }

instance FromJSON CreateCredentialReq where
  parseJSON = withObject "CreateCredentialReq" $ \o ->
    CreateCredentialReq
      <$> o .: "name"
      <*> o .: "system"
      <*> o .: "type"
      <*> o .: "secret"

-- | Response body for GET /api/runs/:id — embeds the Run and its Steps.
data RunDetail = RunDetail
  { rdRun   :: Run
  , rdSteps :: [Step]
  }

instance ToJSON RunDetail where
  toJSON rd = object
    [ "run"   .= rdRun   rd
    , "steps" .= rdSteps rd
    ]

-- ---------------------------------------------------------------------------
-- Error response
-- ---------------------------------------------------------------------------

newtype ApiError = ApiError
  { aeError :: Text
  }

instance ToJSON ApiError where
  toJSON e = object ["error" .= aeError e]

-- ---------------------------------------------------------------------------
-- Spec request / response / error
-- ---------------------------------------------------------------------------

-- | Request body for PUT /api/programs/:id/spec.
newtype SpecRequest = SpecRequest
  { srYaml :: Text
  }

instance FromJSON SpecRequest where
  parseJSON = withObject "SpecRequest" $ \o ->
    SpecRequest <$> o .: "yaml"

-- | Response body for GET /api/programs/:id/spec.
newtype SpecResponse = SpecResponse
  { spYaml :: Text
  }

instance ToJSON SpecResponse where
  toJSON r = object ["yaml" .= spYaml r]

-- | Error body for 422 responses from PUT /api/programs/:id/spec.
newtype SpecError = SpecError
  { seErrors :: [ParseError]
  }

instance ToJSON SpecError where
  toJSON e = object ["errors" .= seErrors e]

-- ---------------------------------------------------------------------------
-- Codebase request bodies
-- ---------------------------------------------------------------------------

-- | Request body for POST /api/programs/:id/codebase (connect a directory).
newtype ConnectCodebaseReq = ConnectCodebaseReq
  { ccrPath :: Text
  }

instance FromJSON ConnectCodebaseReq where
  parseJSON = withObject "ConnectCodebaseReq" $ \o ->
    ConnectCodebaseReq <$> o .: "path"

-- | Request body for PUT /api/codebase/:cbId/file (write file content).
data WriteFileReq = WriteFileReq
  { wfrPath    :: Text
  , wfrContent :: Text
  }

instance FromJSON WriteFileReq where
  parseJSON = withObject "WriteFileReq" $ \o ->
    WriteFileReq
      <$> o .: "path"
      <*> o .: "content"

-- ---------------------------------------------------------------------------
-- Codebase response bodies
-- ---------------------------------------------------------------------------

-- | A single changed file from git status --porcelain.
-- 'gdfStatus' is the raw two-char git porcelain code (e.g. "M ", "??", " D").
data GitDiffFile = GitDiffFile
  { gdfStatus :: Text
  , gdfPath   :: Text
  }

instance ToJSON GitDiffFile where
  toJSON f = object ["status" .= gdfStatus f, "path" .= gdfPath f]

-- | Response body for GET /api/codebase/:cbId/diff.
newtype GitDiffResponse = GitDiffResponse
  { gdrFiles :: [GitDiffFile]
  }

instance ToJSON GitDiffResponse where
  toJSON r = object ["files" .= gdrFiles r]
