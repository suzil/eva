{-# LANGUAGE OverloadedStrings #-}

-- | Action node handler: template variable substitution.
-- Only 'OpTemplate' is implemented in M4. Other operation types
-- (OpCode, OpApiCall, OpFormat) are deferred to later milestones.
module Eva.Engine.Handlers.Action
  ( handleAction
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)

import Eva.App (AppM)
import Eva.Core.Types

-- ---------------------------------------------------------------------------
-- Handler
-- ---------------------------------------------------------------------------

-- | Execute an Action node: substitute template variables from the input
-- message payload and emit an 'action_output' message.
handleAction
  :: RunId
  -> Node
  -> Map PortName Message   -- ^ Consumed data inputs (from mailboxes)
  -> ResourceBindings
  -> AppM Message
handleAction rid node inputs _bindings = do
  -- 1. Extract ActionConfig from the node.
  cfg <- case nodeType node of
    ActionNode c -> pure c
    _            -> liftIO $ throwIO $ userError "handleAction called on a non-Action node"

  -- 2. Dispatch on operation type.
  case actionOperation cfg of
    OpTemplate -> handleTemplate rid node cfg inputs
    op         -> liftIO $ throwIO $ userError $
      "Action operation " <> show op <> " not implemented in M4"

-- ---------------------------------------------------------------------------
-- Template operation
-- ---------------------------------------------------------------------------

handleTemplate
  :: RunId
  -> Node
  -> ActionConfig
  -> Map PortName Message
  -> AppM Message
handleTemplate rid node cfg inputs = do
  -- 1. Extract template string from actionParameters.
  tmpl <- case extractTemplate (actionParameters cfg) of
    Left err -> liftIO $ throwIO $ userError err
    Right t  -> pure t

  -- 2. Extract substitution variables from the first input message payload.
  let vars = case Map.elems inputs of
        []        -> Map.empty
        (msg : _) -> extractVars (msgPayload msg)

  -- 3. Substitute {{varName}} markers.
  result <- case substituteVars tmpl vars of
    Left err -> liftIO $ throwIO $ userError (T.unpack err)
    Right t  -> pure t

  -- 4. Build and return the output message.
  now     <- liftIO getCurrentTime
  traceId <- liftIO (UUID.toText <$> nextRandom)
  let meta = MessageMeta
        { metaTraceId    = traceId
        , metaTimestamp  = now
        , metaSourceNode = nodeId node
        , metaRunId      = rid
        }
  pure $ Message "action_output" (Aeson.String result) meta

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Extract the template string from actionParameters.
-- Accepts a JSON object with a "template" key, or a bare JSON string.
extractTemplate :: Value -> Either String Text
extractTemplate (Aeson.String t) = Right t
extractTemplate (Aeson.Object obj) =
  case KM.lookup "template" obj of
    Just (Aeson.String t) -> Right t
    Just other            -> Left $
      "Action 'template' parameter must be a string, got: " <> show other
    Nothing               -> Left
      "Action config missing 'template' key in parameters"
extractTemplate other = Left $
  "Action parameters must be a string or object, got: " <> show other

-- | Extract substitution variables from a message payload.
-- JSON objects contribute their fields as variables.
-- JSON strings are exposed as the single variable @input@, so a template
-- like @{{input}}@ works naturally when the upstream node (e.g. Agent)
-- emits a plain-text output.
-- Other payload shapes yield an empty map (templates with no markers succeed).
extractVars :: Value -> Map Text Value
extractVars (Aeson.Object obj) =
  Map.fromList [(K.toText k, v) | (k, v) <- KM.toList obj]
extractVars (Aeson.String t) = Map.fromList [("input", Aeson.String t)]
extractVars _ = Map.empty

-- | Pure template substitution.
-- Replaces every @{{varName}}@ marker with the corresponding value from
-- @vars@. Returns @Left errorMsg@ if any marker is not found in the map.
substituteVars :: Text -> Map Text Value -> Either Text Text
substituteVars tmpl vars = go "" tmpl
  where
    go acc t =
      case T.breakOn "{{" t of
        (prefix, "")   -> Right (acc <> prefix)
        (prefix, rest) ->
          case T.breakOn "}}" (T.drop 2 rest) of
            (_, "")           -> Left "Unclosed '{{' in template"
            (varName, suffix) ->
              let key    = T.strip varName
                  after  = T.drop 2 suffix
              in case Map.lookup key vars of
                   Nothing  -> Left $
                     "Missing template variable '{{" <> key <> "}}': not found in input"
                   Just val -> go (acc <> prefix <> valueToText val) after

    valueToText (Aeson.String s) = s
    valueToText v                = T.pack (show v)
