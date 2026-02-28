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
-- 'search_knowledge' is always included as a built-in tool and is handled at
-- the 'runToolLoop' level (not routed to a ConnectorRunner).
module Eva.Engine.Handlers.Agent
  ( handleAgent
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (Value (..), encode, toJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
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
import Eva.App (AppM, broadcastEvent, logMsg, runAppM)
import qualified Eva.App as App
import Eva.Config (LogLevel (..))
import Eva.Core.Types
import Eva.Engine.LLM
import Eva.Knowledge.Query (assembleAgentContext, search)
import Eva.Knowledge.Types (SearchQuery (..), SearchResult (..))
import Eva.Persistence.Queries (getRun)
import Eva.Prompt.Resolve (resolveTemplate)

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

  -- 3. Look up ProgramId from the Run record (needed for auto-knowledge injection).
  mRun <- getRun rid
  let pid = maybe (ProgramId "") runProgramId mRun

  -- 4. Assemble auto-knowledge context from the Knowledge Library and append
  --    to the system prompt (if non-empty). This is programme-scoped: only
  --    entries belonging to this program are considered.
  autoContext <- assembleAgentContext pid (Just (nodeId node))
  let autoContextSection
        | T.null autoContext = ""
        | otherwise          =
            "\n\n# Context from Knowledge Library\n\n" <> autoContext

  -- 5. Build context section from wired Knowledge nodes (inline text).
  let contextTexts   = mapMaybe resolveKnowledgeText (rbKnowledge bindings)
                    ++ map extractText (rbKnowledgeDynamic bindings)
      contextSection = case contextTexts of
        [] -> ""
        ts -> "\n\n## Context\n\n" <> T.intercalate "\n\n---\n\n" ts

  -- 6. Collect ActionSpecs from wired ConnectorRunners (needed before building
  --    the system prompt so we know whether to append failure instructions).
  runnersWithActions <- liftIO $
    mapM (\r -> (r,) <$> connectorAvailableActions r) (rbConnectorRunners bindings)
  let connectorTools = map actionSpecToTool $ concatMap snd runnersWithActions
      actionMap      = Map.fromList
        [ (actionSpecName spec, runner)
        | (runner, specs) <- runnersWithActions
        , spec            <- specs
        ]
      -- search_knowledge is always available as a built-in tool.
      tools = connectorTools ++ [searchKnowledgeTool]

  -- 7. Assemble initial chat messages.
  --    Connector failure instruction is appended when connector tools are present.
  --    Auto-knowledge context is appended to the system prompt (step 4 above).
  --    promptVariableBindings (EVA-99) will supply real bindings; for now use empty map.
  let connectorInstructions =
        "\n\nIf a connector tool returns an error and you cannot complete the " <>
        "task (e.g. authentication failure, service unavailable), respond with " <>
        "exactly:\nTASK_FAILED: <one-line reason>\nDo not add any other text."
      rawSystemPrompt = agentSystemPrompt cfg
        <> autoContextSection
        <> if null connectorTools then "" else connectorInstructions
      (systemPrompt, unresolvedVars) = resolveTemplate rawSystemPrompt Map.empty
      instructionText = extractText (msgPayload instructionMsg)
      userContent     = instructionText <> contextSection
      initMessages    =
        [ ChatMessage "system" systemPrompt
        , ChatMessage "user"   userContent
        ]

  case unresolvedVars of
    [] -> pure ()
    vs -> logMsg LogWarn $
      "Agent system prompt has unresolved variables: " <> T.intercalate ", " vs

  -- 8. Select LLM client based on agentProvider config, then run tool-call loop.
  env <- ask
  let provider  = maybe ProviderOpenAI id (agentProvider cfg)
      llmClient = case provider of
        ProviderOpenAI    -> App.envLLMClient env
        ProviderAnthropic -> App.envAnthropicClient env
  runToolLoop env cfg pid rid node llmClient connectorTools tools actionMap initMessages 0 0.0

-- ---------------------------------------------------------------------------
-- Tool-call loop
-- ---------------------------------------------------------------------------

runToolLoop
  :: App.AppEnv
  -> AgentConfig
  -> ProgramId       -- ^ Programme owning this run (for search_knowledge)
  -> RunId
  -> Node
  -> LLMClient
  -> [ToolSpec]      -- ^ Connector tools only (controls streaming/blocking)
  -> [ToolSpec]      -- ^ All tools passed to the LLM (connector + built-in)
  -> Map Text ConnectorRunner
  -> [ChatMessage]
  -> Int             -- ^ Current iteration
  -> Double          -- ^ Accumulated cost (USD)
  -> AppM Message
runToolLoop env cfg pid rid node llmClient connectorTools tools actionMap messages iteration cost = do

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

  -- Stream when no connector tools (preserves token broadcast UX).
  -- search_knowledge is always present in 'tools' but does not prevent streaming;
  -- the streaming decision is based on connector presence only.
  result <-
    if null connectorTools
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

      -- Execute each tool call: intercept search_knowledge, route the rest
      -- to the connector action map.
      results <- mapM (\tc ->
        if toolCallName tc == "search_knowledge"
          then executeSearchKnowledge pid tc
          else liftIO $ executeToolCall actionMap tc
        ) calls

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

      runToolLoop env cfg pid rid node llmClient connectorTools tools actionMap
                  newMessages (iteration + 1) newCost

    -- Text response (or budget breached — use whatever content we have).
    _ -> do
      now     <- liftIO getCurrentTime
      traceId <- liftIO (UUID.toText <$> nextRandom)
      let outputText = if budgetBreached && T.null (llmContent resp)
                         then "[cost budget exceeded after " <> T.pack (show iteration) <> " iteration(s)]"
                         else llmContent resp
      -- If the LLM signalled an unrecoverable connector failure, fail the step
      -- so the run is marked failed rather than producing a misleading success.
      if "TASK_FAILED:" `T.isPrefixOf` T.stripStart outputText
        then liftIO $ throwIO $ userError $ T.unpack outputText
        else do
          let meta = MessageMeta
                { metaTraceId    = traceId
                , metaTimestamp  = now
                , metaSourceNode = nodeId node
                , metaRunId      = rid
                }
          pure $ Message "agent_output" (toJSON outputText) meta

-- ---------------------------------------------------------------------------
-- Built-in tool: search_knowledge
-- ---------------------------------------------------------------------------

-- | ToolSpec for the built-in knowledge search tool.
-- Always included in every agent's tool list so the LLM can search the
-- programme's Knowledge Library during execution.
searchKnowledgeTool :: ToolSpec
searchKnowledgeTool = ToolSpec
  { toolName        = "search_knowledge"
  , toolDescription =
      "Search the program's knowledge base for relevant information. " <>
      "Returns the top 5 matching entries ranked by relevance."
  , toolParameters  = Aeson.object
      [ "type"       Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
          [ "query"    Aeson..= Aeson.object
              [ "type"        Aeson..= ("string" :: Text)
              , "description" Aeson..= ("Search terms to look up in the knowledge base" :: Text)
              ]
          , "category" Aeson..= Aeson.object
              [ "type"        Aeson..= ("string" :: Text)
              , "description" Aeson..= ("Optional category filter: structure, metadata, pattern, summary, or reference" :: Text)
              ]
          ]
      , "required"   Aeson..= (["query"] :: [Text])
      ]
  }

-- | Execute a search_knowledge tool call in AppM.
-- Parses the 'query' argument from the tool call args, runs an FTS5 search
-- scoped to the program, and returns up to 5 entries as a JSON array.
executeSearchKnowledge :: ProgramId -> ToolCall -> AppM Value
executeSearchKnowledge pid tc = do
  let args   = toolCallArgs tc
      mQuery = parseMaybe (Aeson.withObject "args" (Aeson..: "query")) args :: Maybe Text
  case mQuery of
    Nothing ->
      pure $ toJSON ("search_knowledge: missing required 'query' argument" :: Text)
    Just q -> do
      results <- search SearchQuery
        { searchQueryText       = q
        , searchQuerySourceType = Nothing
        , searchQueryCategory   = Nothing
        , searchQueryProgramId  = Just pid
        , searchQueryLimit      = Just 5
        }
      pure . toJSON $ map searchResultEntry results

-- ---------------------------------------------------------------------------
-- Connector tool execution
-- ---------------------------------------------------------------------------

executeToolCall :: Map Text ConnectorRunner -> ToolCall -> IO Value
executeToolCall actionMap tc = do
  case Map.lookup (toolCallName tc) actionMap of
    Nothing ->
      pure $ toJSON $ ("unknown tool: " <> toolCallName tc :: Text)
    Just runner -> do
      res <- connectorExecuteAction runner (ActionName (toolCallName tc)) (toolCallArgs tc)
      case res of
        Right v  -> pure v
        -- Hard failures: credential/config problems the LLM cannot fix.
        Left err@(ConnectorMissingCredential _) -> throwIO (userError (T.unpack (connectorErrorText err)))
        Left err@(ConnectorInvalidCredential _) -> throwIO (userError (T.unpack (connectorErrorText err)))
        Left err@(ConnectorUnsupported _)       -> throwIO (userError (T.unpack (connectorErrorText err)))
        -- Soft failures: API/network errors are passed back to the LLM as a
        -- tool result so it can note the issue or adapt its response.
        Left err                                -> pure (toJSON (connectorErrorText err))

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
