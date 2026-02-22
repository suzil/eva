{-# LANGUAGE OverloadedStrings #-}

-- | Agent node handler: prompt assembly, LLM invocation, output emission.
-- Receives a populated 'ResourceBindings' (resolved by the Runner before dispatch)
-- containing all wired Knowledge and Connector configs.
-- Tool use is STUBBED in M4 — real tool-call loop is EVA-33.
module Eva.Engine.Handlers.Agent
  ( handleAgent
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)

import Eva.Api.WebSocket (llmTokenEvent)
import Eva.App (AppM, broadcastEvent, runAppM)
import qualified Eva.App as App
import Eva.Core.Types
import Eva.Engine.LLM

-- ---------------------------------------------------------------------------
-- Handler
-- ---------------------------------------------------------------------------

-- | Execute an Agent node: assemble prompt, call LLM, emit output message.
handleAgent
  :: RunId
  -> Node
  -> Map PortName Message   -- ^ Consumed data inputs (from mailboxes)
  -> ResourceBindings
  -> AppM Message
handleAgent rid node inputs bindings = do
  -- 1. Extract required 'instruction' data input.
  instructionMsg <- case Map.lookup "instruction" inputs of
    Nothing ->
      liftIO $ throwIO $ userError
        "Agent node missing required 'instruction' input: no message on 'instruction' port"
    Just m -> pure m

  -- 2. Extract AgentConfig from the node.
  cfg <- case nodeType node of
    AgentNode c -> pure c
    _           -> liftIO $ throwIO $ userError "handleAgent called on a non-Agent node"

  -- 3. Build context section from wired Knowledge nodes (inline content only in M4).
  let contextTexts   = mapMaybe resolveKnowledgeText (rbKnowledge bindings)
      contextSection = case contextTexts of
        [] -> ""
        ts -> "\n\n## Context\n\n" <> T.intercalate "\n\n---\n\n" ts

  -- 4. Assemble chat messages.
  let instructionText = extractText (msgPayload instructionMsg)
      userContent     = instructionText <> contextSection
      messages        =
        [ ChatMessage "system" (agentSystemPrompt cfg)
        , ChatMessage "user"   userContent
        ]

  -- 5. Build the LLM request.
  let llmReq = LLMRequest
        { llmModel          = agentModel cfg
        , llmMessages       = messages
        , llmTemperature    = agentTemperature cfg
        , llmMaxTokens      = agentMaxTokens cfg
        , llmResponseFormat = agentResponseFormat cfg
        }

  -- 6. Call the LLM with streaming; broadcast each token as an llm_token event.
  env       <- ask
  llmClient <- asks App.envLLMClient
  let onToken tok = do
        now <- getCurrentTime
        runAppM env $ broadcastEvent rid (llmTokenEvent rid (nodeId node) tok now)
  result    <- liftIO $ clientStream llmClient llmReq onToken

  -- 7. Build and return the output message.
  case result of
    Left err -> liftIO $ throwIO $ userError (show err)
    Right resp -> do
      now     <- liftIO getCurrentTime
      traceId <- liftIO (UUID.toText <$> nextRandom)
      let meta = MessageMeta
            { metaTraceId    = traceId
            , metaTimestamp  = now
            , metaSourceNode = nodeId node
            , metaRunId      = rid
            }
      pure $ Message "agent_output" (toJSON (llmContent resp)) meta

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Extract a plain-text representation from a message payload.
-- JSON strings are unwrapped; other values are shown as-is.
extractText :: Value -> Text
extractText (Aeson.String t) = t
extractText v                = T.pack (show v)

-- | Resolve a KnowledgeConfig to its inline text content.
-- Only 'InlineText' is handled in M4; file/URL/upstream sources return Nothing.
resolveKnowledgeText :: KnowledgeConfig -> Maybe Text
resolveKnowledgeText cfg =
  case knowledgeSource cfg of
    InlineText t -> Just t
    _            -> Nothing
