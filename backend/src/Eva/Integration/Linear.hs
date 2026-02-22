{-# LANGUAGE OverloadedStrings #-}

-- | Linear connector: list, create, and update issues via the Linear GraphQL API.
-- EVA-30: mkLinearRunner stub — returns empty action list; EVA-31 fills real calls.
module Eva.Integration.Linear
  ( mkLinearRunner
  ) where

import Data.ByteString (ByteString)

import Eva.Core.Types (ConnectorConfig)
import Eva.Integration.Types

-- | Construct a ConnectorRunner for the Linear system.
-- @apiKey@ is the decrypted API key bytes (UTF-8 encoded Linear API token).
-- EVA-31 replaces the stubs with real Linear GraphQL calls.
mkLinearRunner :: ByteString -> ConnectorConfig -> ConnectorRunner
mkLinearRunner _apiKey _cfg = ConnectorRunner
  { connectorAvailableActions = pure []
    -- EVA-31: return list/create/update issue ActionSpecs
  , connectorExecuteAction    = \_ _ ->
      pure (Left (ConnectorApiError "Linear connector not yet implemented (EVA-31)"))
    -- EVA-31: dispatch to real Linear API calls
  }
