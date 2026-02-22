{-# LANGUAGE OverloadedStrings #-}

-- | API request/response JSON types (distinct from domain types).
-- Domain types (Program, Graph, Node, Edge) are used directly as response
-- bodies where possible; these types cover request bodies and error shapes.
module Eva.Api.Types
  ( -- * Request bodies
    CreateProgramReq (..)
  , PatchProgramReq (..)

    -- * Response bodies
  , HealthResponse (..)
  , ValidateResult (..)

    -- * Error response
  , ApiError (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)

import Eva.Core.Types (ValidationError (..))

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

-- ---------------------------------------------------------------------------
-- Error response
-- ---------------------------------------------------------------------------

newtype ApiError = ApiError
  { aeError :: Text
  }

instance ToJSON ApiError where
  toJSON e = object ["error" .= aeError e]
