{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Connector framework types: ActionSpec, ActionName, ConnectorError, ConnectorRunner.
-- ConnectorRunner is a record-of-functions (analogous to LLMClient) that encapsulates
-- a resolved connector: available actions enumeration and action execution dispatch.
-- EVA-30: framework types + credential wiring. EVA-31: Linear implementation.
--
-- This module has NO Eva-internal imports to avoid circular dependencies
-- (Core.Types imports Integration.Types for ConnectorRunner).
module Eva.Integration.Types
  ( -- * Action specification
    ActionSpec (..)
  , ActionName (..)

    -- * Connector interface
  , ConnectorRunner (..)

    -- * Error type
  , ConnectorError (..)
  , connectorErrorText
  ) where

import Data.Aeson
import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- Local helper (mirrors dropPrefix in Core.Types — cannot import it)
-- ---------------------------------------------------------------------------

dropPrefixOpts :: String -> Options
dropPrefixOpts prefix =
  defaultOptions
    { fieldLabelModifier = lowerFirst . drop (length prefix)
    , omitNothingFields  = True
    }
  where
    lowerFirst []       = []
    lowerFirst (c : cs) = toLower c : cs

-- ---------------------------------------------------------------------------
-- Action specification
-- ---------------------------------------------------------------------------

-- | Describes a single action a Connector can perform.
-- Used to generate tool schemas for LLM tool-calling (EVA-33).
data ActionSpec = ActionSpec
  { actionSpecName        :: Text
  , actionSpecDescription :: Text
  , actionSpecParameters  :: Value  -- ^ JSON Schema object describing parameters
  , actionSpecReturnType  :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ActionSpec where
  toJSON = genericToJSON (dropPrefixOpts "actionSpec")
  toEncoding = genericToEncoding (dropPrefixOpts "actionSpec")

instance FromJSON ActionSpec where
  parseJSON = genericParseJSON (dropPrefixOpts "actionSpec")

-- | Identifies an action to execute on a Connector.
newtype ActionName = ActionName Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Connector error
-- ---------------------------------------------------------------------------

data ConnectorError
  = ConnectorMissingCredential Text
    -- ^ Connector node has no credentialId configured.
  | ConnectorInvalidCredential Text
    -- ^ Credential found but decryption or validation failed.
  | ConnectorUnsupported Text
    -- ^ System type not yet implemented (post-MLP).
  | ConnectorApiError Text
    -- ^ Runtime error calling the external API.
  deriving stock (Eq, Show, Generic)

-- | Human-readable description for logging and error propagation.
connectorErrorText :: ConnectorError -> Text
connectorErrorText (ConnectorMissingCredential msg) = "missing credential: " <> msg
connectorErrorText (ConnectorInvalidCredential msg) = "invalid credential: " <> msg
connectorErrorText (ConnectorUnsupported msg)        = "unsupported connector: " <> msg
connectorErrorText (ConnectorApiError msg)           = "connector API error: " <> msg

instance ToJSON ConnectorError where
  toJSON e = object ["error" .= connectorErrorText e]

-- ---------------------------------------------------------------------------
-- Connector runner
-- ---------------------------------------------------------------------------

-- | A resolved, ready-to-use connector.
-- Constructed by 'Eva.Engine.Handlers.Connector.resolveConnectorRunner'
-- after credential lookup. Uses IO (not AppM) to keep this module
-- free of the AppM dependency — same pattern as LLMClient.
data ConnectorRunner = ConnectorRunner
  { connectorAvailableActions :: IO [ActionSpec]
    -- ^ Return all actions this connector exposes (filtered by connectorActionFilter).
  , connectorExecuteAction    :: ActionName -> Value -> IO (Either ConnectorError Value)
    -- ^ Execute a named action with a JSON argument object.
  }
