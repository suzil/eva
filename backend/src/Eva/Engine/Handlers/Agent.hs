{-# LANGUAGE OverloadedStrings #-}

-- | Agent node handler: prompt assembly, LLM invocation, tool-call loop.
-- Receives a populated 'ResourceBindings' (resolved by the Runner before dispatch)
-- containing all wired Knowledge and Connector configs/runners.
--
-- When connectors are wired the handler enters a multi-turn tool-call loop:
--   1. Collect ActionSpecs from each ConnectorRunner.
--   2. Convert to 'ToolSpec' (OpenAI function format) and build an action map.
--   3. Loop: call LLM → if finish_reason=tool_calls → execute → feed results back.
--   4. Stop when finish_reason=stop, max iterations reached, or cost budget exceeded.
--
-- When no connectors are wired, the handler falls back to the original
-- single-turn streaming path ('clientStream'), preserving token broadcast UX.
module Eva.Engine.Handlers.Agent
  ( handleAgent
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (Value (..), encode, toJSON)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)

import Eva.Api.WebSocket (llmTokenEvent, toolCallEvent)
import Eva.App (AppM, broadcastEvent, runAppM)
import qualified Eva.App as App
import Eva.Core.Types
import Eva.Engine.LLM

-- ---------------------------------------------------------------------------
-- Handler
-- ---------------------------------------------------------------------------

-- | Execute an Agent node: assemble prompt, run tool-call loop, emit output.
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

  -- 3. Build context section from wired Knowledge nodes.
  let contextTexts   = mapMaybe resolveKnowledgeText (rbKnowledge bindings)
                    ++ map extractText (rbKnowledgeDynamic bindings)
      contextSection = case contextTexts of
        [] -> ""
        ts -> "\n\n## Context\n\n" <> T.intercalate "\n\n---\n\n" ts

  -- 4. Assemble initial chat messages.
  let instructionText = extractText (msgPayload instructionMsg)
      userContent     = instructionText <> contextSection
      initMessages    =
        [ ChatMessage "system" (agentSystemPrompt cfg)
        , ChatMessage "user"   userContent
        ]

  -- 5. Collect ActionSpecs from wired ConnectorRunners.
  runnersWithActions <- liftIO $
    mapM (\r -> (r,) <$> connectorAvailableActions r) (rbConnectorRunners bindings)
  let tools     = map actionSpecToTool $ concatMap snd runnersWithActions
      actionMap = Map.fromList
        [ (actionSpecName spec, runner)
        | (runner, specs) <- runnersWithActions
        , spec            <- specs
        ]

  -- 6. Select LLM client based on agentProvider config, then run tool-call loop.
  env       <- ask
  let provider  = maybe ProviderOpenAI id (agentProvider cfg)
      llmClient = case provider of
        ProviderOpenAI    -> App.envLLMClient env
        ProviderAnthropic -> App.envAnthropicClient env
  runToolLoop env cfg rid node llmClient tools actionMap initMessages 0 0.0

-- ---------------------------------------------------------------------------
-- Tool-call loop
-- ---------------------------------------------------------------------------

runToolLoop
  :: App.AppEnv
  -> AgentConfig
  -> RunId
  -> Node
  -> LLMClient
  -> [ToolSpec]
  -> Map Text ConnectorRunner
  -> [ChatMessage]
  -> Int     -- ^ current iteration
  -> Double  -- ^ accumulated cost (USD)
  -> AppM Message
runToolLoop env cfg rid node llmClient tools actionMap messages iteration cost = do

  -- Guard: max iterations
  let maxIter = agentMaxIterations cfg
  if iteration >= maxIter
    then liftIO $ throwIO $ userError $
           "Agent exceeded max_iterations (" <> show maxIter <> ")"
    else pure ()

  -- Build LLM request
  let llmReq = LLMRequest
        { llmModel          = agentModel cfg
        , llmMessages       = messages
        , llmTemperature    = agentTemperature cfg
        , llmMaxTokens      = agentMaxTokens cfg
        , llmResponseFormat = agentResponseFormat cfg
        , llmTools          = tools
        }

  -- Call LLM: streaming when no tools (preserves token broadcast UX),
  -- blocking when tools are present (intermediate rounds produce no user text).
  result <-
    if null tools
      then do
        let onToken tok = do
              now <- getCurrentTime
              runAppM env $ broadcastEvent rid (llmTokenEvent rid (nodeId node) tok now)
        liftIO $ clientStream llmClient llmReq onToken
      else liftIO $ clientCall llmClient llmReq

  resp <- case result of
    Left err -> liftIO $ throwIO $ userError (show err)
    Right r  -> pure r

  -- Check cost budget after this round
  let iterCost    = estimateCost (agentModel cfg) (llmUsage resp)
      newCost     = cost + iterCost
      budgetBreached = case agentCostBudgetUsd cfg of
        Just budget -> newCost >= budget
        Nothing     -> False

  case llmToolCalls resp of
    -- Tool-call round: execute tools, append messages, recurse.
    Just calls | not budgetBreached -> do
      -- Broadcast tool invocations
      mapM_ (\tc -> do
        now <- liftIO getCurrentTime
        broadcastEvent rid (toolCallEvent rid (nodeId node) "invoke"
          (toJSON (Map.fromList
            [ ("tool_call_id" :: Text, toJSON (toolCallId tc))
            , ("function",            toJSON (toolCallName tc))
            , ("arguments",           toolCallArgs tc)
            ])) now)
        ) calls

      -- Execute each tool call
      results <- liftIO $ mapM (executeToolCall actionMap) calls

      -- Broadcast tool results
      mapM_ (\(tc, res) -> do
        now <- liftIO getCurrentTime
        broadcastEvent rid (toolCallEvent rid (nodeId node) "result"
          (toJSON (Map.fromList
            [ ("tool_call_id" :: Text, toJSON (toolCallId tc))
            , ("result",              res)
            ])) now)
        ) (zip calls results)

      -- Append assistant tool-call message and tool result messages
      let toolCallMsg  = ToolCallMsg calls
          toolResultMsgs =
            [ ToolResultMsg (toolCallId tc) (renderToolResult r)
            | (tc, r) <- zip calls results
            ]
          newMessages = messages ++ [toolCallMsg] ++ toolResultMsgs

      runToolLoop env cfg rid node llmClient tools actionMap
                  newMessages (iteration + 1) newCost

    -- Text response (or budget breached — use whatever content we have).
    _ -> do
      now     <- liftIO getCurrentTime
      traceId <- liftIO (UUID.toText <$> nextRandom)
      let outputText = if budgetBreached && T.null (llmContent resp)
                         then "[cost budget exceeded after " <> T.pack (show iteration) <> " iteration(s)]"
                         else llmContent resp
          meta = MessageMeta
            { metaTraceId    = traceId
            , metaTimestamp  = now
            , metaSourceNode = nodeId node
            , metaRunId      = rid
            }
      pure $ Message "agent_output" (toJSON outputText) meta

-- ---------------------------------------------------------------------------
-- Tool execution
-- ---------------------------------------------------------------------------

executeToolCall :: Map Text ConnectorRunner -> ToolCall -> IO Value
executeToolCall actionMap tc = do
  case Map.lookup (toolCallName tc) actionMap of
    Nothing ->
      pure $ toJSON $ ("unknown tool: " <> toolCallName tc :: Text)
    Just runner -> do
      res <- connectorExecuteAction runner (ActionName (toolCallName tc)) (toolCallArgs tc)
      pure $ case res of
        Right v  -> v
        Left err -> toJSON (connectorErrorText err)

renderToolResult :: Value -> Text
renderToolResult (Aeson.String t) = t
renderToolResult v                = TL.toStrict (TLE.decodeUtf8 (encode v))

-- ---------------------------------------------------------------------------
-- ActionSpec → ToolSpec conversion
-- ---------------------------------------------------------------------------

actionSpecToTool :: ActionSpec -> ToolSpec
actionSpecToTool spec = ToolSpec
  { toolName        = actionSpecName spec
  , toolDescription = actionSpecDescription spec
  , toolParameters  = actionSpecParameters spec
  }

-- ---------------------------------------------------------------------------
-- Cost estimation
-- ---------------------------------------------------------------------------

-- | Rough cost estimate in USD using hardcoded OpenAI rates.
-- Falls back to GPT-4o rates for unknown models.
estimateCost :: Text -> TokenUsage -> Double
estimateCost model usage =
  let promptTok     = fromIntegral (usagePromptTokens usage)
      completionTok = fromIntegral (usageCompletionTokens usage)
      (promptRate, completionRate) = modelRates model
  in  promptTok * promptRate + completionTok * completionRate

-- | (prompt $/token, completion $/token) for known models.
modelRates :: Text -> (Double, Double)
modelRates m
  -- OpenAI models
  | "gpt-4o-mini"    `T.isInfixOf` m = (0.15  / 1_000_000,  0.60 / 1_000_000)
  | "gpt-4o"         `T.isInfixOf` m = (5.00  / 1_000_000, 15.00 / 1_000_000)
  | "gpt-4-turbo"    `T.isInfixOf` m = (10.00 / 1_000_000, 30.00 / 1_000_000)
  | "gpt-3.5"        `T.isInfixOf` m = (0.50  / 1_000_000,  1.50 / 1_000_000)
  -- Anthropic models
  | "claude-opus"    `T.isInfixOf` m = (15.00 / 1_000_000, 75.00 / 1_000_000)
  | "claude-3-5-haiku" `T.isInfixOf` m = (0.80 / 1_000_000,  4.00 / 1_000_000)
  | "claude-3-5-sonnet" `T.isInfixOf` m = (3.00 / 1_000_000, 15.00 / 1_000_000)
  | "claude-sonnet-4"  `T.isInfixOf` m = (3.00 / 1_000_000, 15.00 / 1_000_000)
  | "claude-haiku-4"   `T.isInfixOf` m = (0.80 / 1_000_000,  4.00 / 1_000_000)
  | otherwise                          = (5.00 / 1_000_000, 15.00 / 1_000_000)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Extract a plain-text representation from a message payload.
extractText :: Value -> Text
extractText (Aeson.String t) = t
extractText v                = T.pack (show v)

-- | Resolve a KnowledgeConfig to its inline text content.
resolveKnowledgeText :: KnowledgeConfig -> Maybe Text
resolveKnowledgeText cfg =
  case knowledgeSource cfg of
    InlineText t -> Just t
    _            -> Nothing
