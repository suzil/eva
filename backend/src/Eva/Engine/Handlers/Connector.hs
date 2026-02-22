{-# LANGUAGE OverloadedStrings #-}

-- | Connector handler: resolves a ConnectorConfig into a live ConnectorRunner.
-- Fetches and decrypts the credential, then dispatches to the appropriate
-- system-specific runner factory. Used by the Runner to populate
-- ResourceBindings.rbConnectorRunners before Agent dispatch (EVA-33).
module Eva.Engine.Handlers.Connector
  ( resolveConnectorRunner
  ) where

import qualified Data.Text as T

import Eva.App (AppM)
import Eva.Core.Types
import qualified Eva.Integration.Linear as Linear
import Eva.Persistence.Queries (getDecryptedCredential)

-- | Resolve a ConnectorConfig into a ConnectorRunner.
-- Steps:
--   1. Check that connectorCredentialId is set.
--   2. Fetch and decrypt the credential secret from the DB.
--   3. Dispatch on connectorSystem to construct the system-specific runner.
--   4. Apply connectorActionFilter (if non-empty) to limit available actions.
resolveConnectorRunner
  :: ConnectorConfig
  -> AppM (Either ConnectorError ConnectorRunner)
resolveConnectorRunner cfg = do
  case connectorCredentialId cfg of
    Nothing ->
      pure $ Left $ ConnectorMissingCredential
        "connector node has no credential configured (set connectorCredentialId)"

    Just rawId -> do
      let cid = CredentialId rawId
      result <- getDecryptedCredential cid
      case result of
        Left err ->
          pure $ Left $ ConnectorInvalidCredential (T.pack err)

        Right secretBytes ->
          pure $ Right $ applyActionFilter cfg $ mkRunner secretBytes
  where
    mkRunner secretBytes =
      case connectorSystem cfg of
        SystemLinear   -> Linear.mkLinearRunner secretBytes cfg
        SystemGitHub   -> stubRunner "github"
        SystemHttp     -> stubRunner "http"
        SystemCodebase -> stubRunner "codebase"

    stubRunner name = ConnectorRunner
      { connectorAvailableActions = pure []
      , connectorExecuteAction    = \_ _ ->
          pure $ Left $ ConnectorUnsupported $
            name <> " connector not yet implemented"
      }

-- | Wrap a ConnectorRunner so that connectorAvailableActions only returns
-- actions whose names appear in the filter list.
-- If the filter list is empty, all actions are returned.
applyActionFilter :: ConnectorConfig -> ConnectorRunner -> ConnectorRunner
applyActionFilter cfg runner =
  case connectorActionFilter cfg of
    [] -> runner
    allowed ->
      let allowedSet = map ActionName allowed
      in runner
           { connectorAvailableActions = do
               specs <- connectorAvailableActions runner
               pure [ s | s <- specs, ActionName (actionSpecName s) `elem` allowedSet ]
           }
