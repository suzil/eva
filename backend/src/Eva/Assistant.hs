{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | MAGI AI assistant: types, tool specs, system prompt, and conversation loop.
--
-- EVA-84 contributed the pure declarations (context, messages, diff, tool specs,
-- system prompt). EVA-85 adds the LLM dispatch loop and all tool implementations.
-- EVA-86 wires this into the WebSocket @assistant:<convId>@ topic.
module Eva.Assistant
  ( -- * Conversation
    ConversationId (..)
  , handleAssistantMessage

    -- * Context
  , AssistantContext (..)
  , GraphSummary (..)
  , NodeSummary (..)
  , ProgramSummary (..)

    -- * Messages
  , AssistantMessage (..)

    -- * Graph diff
  , GraphDiff (..)
  , NodeModification (..)

    -- * LLM tools
  , assistantTools

    -- * System prompt
  , magiSystemPrompt
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)

import Eva.Api.Types (RunDetail (..))
import qualified Eva.App as App
import Eva.App (AppM)
import Eva.Core.Types
  ( EdgeId
  , Edge (..)
  , Graph (..)
  , Node (..)
  , NodeId (..)
  , ProgramId (..)
  , ProgramState
  , Program (..)
  , ResponseFormat (..)
  , RunId (..)
  , ValidationError (..)
  )
import Eva.Core.Validation (validateGraph)
import Eva.Engine.LLM
  ( LLMClient (..)
  , LLMRequest (..)
  , LLMResponse (..)
  , ChatMessage (..)
  , ToolCall (..)
  , ToolSpec (..)
  )
import Eva.Persistence.Queries
  ( getProgram
  , listPrograms
  , getRun
  , listStepsForRun
  )

-- ---------------------------------------------------------------------------
-- ConversationId
-- ---------------------------------------------------------------------------

-- | Opaque identifier for an assistant conversation thread.
-- One thread per program; key: @eva:assistant:conversations:{programId}@ in
-- the frontend. The WS topic is @assistant:<conversationId>@ (EVA-86).
newtype ConversationId = ConversationId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Local helpers
-- ---------------------------------------------------------------------------

-- | Mirror of the unexported 'dropPrefix' in Eva.Core.Types.
-- Strips the given camelCase prefix from field labels and lowercases the
-- first remaining character. 'omitNothingFields' is set so optional fields
-- are absent rather than @null@ in JSON.
dropPfx :: String -> Options
dropPfx prefix =
  defaultOptions
    { fieldLabelModifier = lowerFirst . drop (length prefix)
    , omitNothingFields  = True
    }
  where
    lowerFirst []       = []
    lowerFirst (c : cs) = toLower c : cs

-- ---------------------------------------------------------------------------
-- Support records
-- ---------------------------------------------------------------------------

-- | Compact summary of a program's graph, included automatically in every
-- assistant request to give the LLM quick structural context.
data GraphSummary = GraphSummary
  { gsNodeCount :: Int
  , gsEdgeCount :: Int
  , gsNodeTypes :: [Text]   -- ^ e.g. ["agent","trigger","knowledge"]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GraphSummary where
  toJSON     = genericToJSON     (dropPfx "gs")
  toEncoding = genericToEncoding (dropPfx "gs")

instance FromJSON GraphSummary where
  parseJSON = genericParseJSON (dropPfx "gs")

-- | Summary of the currently-selected node on the canvas.
data NodeSummary = NodeSummary
  { nsId    :: NodeId
  , nsLabel :: Text
  , nsType  :: Text   -- ^ "agent" | "knowledge" | "connector" | "action" | "trigger"
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON NodeSummary where
  toJSON     = genericToJSON     (dropPfx "ns")
  toEncoding = genericToEncoding (dropPfx "ns")

instance FromJSON NodeSummary where
  parseJSON = genericParseJSON (dropPfx "ns")

-- | Lightweight program entry for the program list in context.
data ProgramSummary = ProgramSummary
  { psId    :: ProgramId
  , psName  :: Text
  , psState :: ProgramState
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ProgramSummary where
  toJSON     = genericToJSON     (dropPfx "ps")
  toEncoding = genericToEncoding (dropPfx "ps")

instance FromJSON ProgramSummary where
  parseJSON = genericParseJSON (dropPfx "ps")

-- ---------------------------------------------------------------------------
-- AssistantContext
-- ---------------------------------------------------------------------------

-- | The context bundle sent with every assistant message.
-- The frontend populates this from its current UI state before each request.
-- Fields are optional so the frontend can omit irrelevant sections.
data AssistantContext = AssistantContext
  { ctxProgramId    :: Maybe ProgramId
  , ctxProgramName  :: Maybe Text
  , ctxProgramState :: Maybe ProgramState
  , ctxGraphSummary :: Maybe GraphSummary
  , ctxSelectedNode :: Maybe NodeSummary
  , ctxCurrentMode  :: Text              -- ^ "author" | "operate"
  , ctxActiveRunId  :: Maybe RunId
  , ctxRecentErrors :: [Text]
  , ctxProgramList  :: [ProgramSummary]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AssistantContext where
  toJSON     = genericToJSON     (dropPfx "ctx")
  toEncoding = genericToEncoding (dropPfx "ctx")

instance FromJSON AssistantContext where
  parseJSON = genericParseJSON (dropPfx "ctx")

-- ---------------------------------------------------------------------------
-- GraphDiff
-- ---------------------------------------------------------------------------

-- | A proposed modification to a single node's config.
-- 'before' and 'after' are partial JSON objects (only changed fields present).
data NodeModification = NodeModification
  { nmNodeId :: NodeId
  , nmBefore :: Value
  , nmAfter  :: Value
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON NodeModification where
  toJSON     = genericToJSON     (dropPfx "nm")
  toEncoding = genericToEncoding (dropPfx "nm")

instance FromJSON NodeModification where
  parseJSON = genericParseJSON (dropPfx "nm")

-- | A typed diff describing the changes MAGI proposes to an existing graph.
-- Sent to the frontend as an 'AsstGraphDiff' message; the frontend renders
-- a visual overlay and prompts the user to accept or reject.
data GraphDiff = GraphDiff
  { gdAddedNodes     :: [Node]
  , gdRemovedNodeIds :: [NodeId]
  , gdModifiedNodes  :: [NodeModification]
  , gdAddedEdges     :: [Edge]
  , gdRemovedEdgeIds :: [EdgeId]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GraphDiff where
  toJSON     = genericToJSON     (dropPfx "gd")
  toEncoding = genericToEncoding (dropPfx "gd")

instance FromJSON GraphDiff where
  parseJSON = genericParseJSON (dropPfx "gd")

-- ---------------------------------------------------------------------------
-- AssistantMessage
-- ---------------------------------------------------------------------------

-- | A message in an assistant conversation thread.
-- Uses manual Aeson instances to give exact control over the @"type"@
-- discriminator and field names. The frontend discriminates on @type@
-- to render the appropriate message component.
data AssistantMessage
  = AsstUser          Text
    -- ^ Message sent by the user. @{ "type": "user", "text": "..." }@
  | AsstText          Text
    -- ^ Plain markdown text response from MAGI.
  | AsstGraphProposal Graph  Text
    -- ^ A new graph MAGI proposes to create. Frontend shows a preview overlay.
  | AsstGraphDiff     GraphDiff Text
    -- ^ Proposed modifications to the current graph.
  | AsstNodeRef       NodeId Text
    -- ^ An inline node reference chip — clickable on the canvas.
  | AsstRunData       RunId  Text
    -- ^ An embedded run summary card with expandable step detail.
  | AsstActionConfirm Text   Text
    -- ^ An operation MAGI wants to perform; requires user confirmation.
  deriving stock (Eq, Show)

instance ToJSON AssistantMessage where
  toJSON (AsstUser txt) =
    object ["type" .= ("user" :: Text), "text" .= txt]
  toJSON (AsstText txt) =
    object ["type" .= ("text" :: Text), "text" .= txt]
  toJSON (AsstGraphProposal graph summary) =
    object
      [ "type"    .= ("graph_proposal" :: Text)
      , "graph"   .= graph
      , "summary" .= summary
      ]
  toJSON (AsstGraphDiff diff summary) =
    object
      [ "type"    .= ("graph_diff" :: Text)
      , "diff"    .= diff
      , "summary" .= summary
      ]
  toJSON (AsstNodeRef nodeId label) =
    object
      [ "type"   .= ("node_ref" :: Text)
      , "nodeId" .= nodeId
      , "label"  .= label
      ]
  toJSON (AsstRunData runId summary) =
    object
      [ "type"    .= ("run_data" :: Text)
      , "runId"   .= runId
      , "summary" .= summary
      ]
  toJSON (AsstActionConfirm operation description) =
    object
      [ "type"        .= ("action_confirm" :: Text)
      , "operation"   .= operation
      , "description" .= description
      ]

instance FromJSON AssistantMessage where
  parseJSON = withObject "AssistantMessage" $ \o -> do
    msgType <- o .: "type" :: Parser Text
    case msgType of
      "user"           -> AsstUser          <$> o .: "text"
      "text"           -> AsstText          <$> o .: "text"
      "graph_proposal" -> AsstGraphProposal <$> o .: "graph"   <*> o .: "summary"
      "graph_diff"     -> AsstGraphDiff     <$> o .: "diff"    <*> o .: "summary"
      "node_ref"       -> AsstNodeRef       <$> o .: "nodeId"  <*> o .: "label"
      "run_data"       -> AsstRunData       <$> o .: "runId"   <*> o .: "summary"
      "action_confirm" -> AsstActionConfirm <$> o .: "operation" <*> o .: "description"
      other            -> fail $ "Unknown AssistantMessage type: " <> show other

-- ---------------------------------------------------------------------------
-- LLM tool specs
-- ---------------------------------------------------------------------------

-- | All 7 tools available to the MAGI assistant LLM.
-- These are passed as 'Eva.Engine.LLM.ToolSpec' values in each 'LLMRequest'.
-- 'toolSpecToJson' in 'Eva.Engine.LLM' wraps them in the OpenAI
-- function-calling envelope @{ "type": "function", "function": { ... } }@.
assistantTools :: [ToolSpec]
assistantTools =
  [ ToolSpec
      { toolName        = "get_graph"
      , toolDescription = "Retrieve the full graph (all nodes with configs, all edges) for a program."
      , toolParameters  = objSchema
          [("programId", strProp "The program ID to retrieve the graph for.")]
          ["programId"]
      }
  , ToolSpec
      { toolName        = "get_node"
      , toolDescription = "Retrieve the configuration of a single node by its ID."
      , toolParameters  = objSchema
          [("nodeId", strProp "The node ID to retrieve.")]
          ["nodeId"]
      }
  , ToolSpec
      { toolName        = "get_run_detail"
      , toolDescription = "Retrieve a run and all its step results, states, and error messages."
      , toolParameters  = objSchema
          [("runId", strProp "The run ID to retrieve.")]
          ["runId"]
      }
  , ToolSpec
      { toolName        = "search_programs"
      , toolDescription = "Search programs by name or content. Returns matching program summaries."
      , toolParameters  = objSchema
          [("query", strProp "Natural-language or keyword search query.")]
          ["query"]
      }
  , ToolSpec
      { toolName        = "propose_graph"
      , toolDescription =
          "Propose a new program graph for the user to preview and accept. "
          <> "The 'graph' must be valid Eva Graph JSON (nodes map + edges list). "
          <> "The 'summary' is a short human-readable description of what the graph does."
      , toolParameters  = objSchema
          [ ("graph",   object ["type" .= ("object" :: Text), "description" .= ("Complete Eva Graph JSON with nodes and edges." :: Text)])
          , ("summary", strProp "One-sentence description of the proposed program.")
          ]
          ["graph", "summary"]
      }
  , ToolSpec
      { toolName        = "propose_diff"
      , toolDescription =
          "Propose modifications to the current program graph. "
          <> "The 'diff' describes added/removed nodes and edges, and config changes. "
          <> "The 'summary' explains the change to the user."
      , toolParameters  = objSchema
          [ ("diff",    object ["type" .= ("object" :: Text), "description" .= ("GraphDiff JSON describing the proposed changes." :: Text)])
          , ("summary", strProp "One-sentence description of the proposed change.")
          ]
          ["diff", "summary"]
      }
  , ToolSpec
      { toolName        = "execute_operation"
      , toolDescription =
          "Execute a lifecycle operation on a program: deploy, run, pause, resume, or status. "
          <> "Always confirm with the user before calling this tool."
      , toolParameters  = objSchema
          [ ( "operation"
            , object
                [ "type"        .= ("string" :: Text)
                , "description" .= ("Operation to perform." :: Text)
                , "enum"        .= (["deploy", "run", "pause", "resume", "status"] :: [Text])
                ]
            )
          , ("programId", strProp "The program ID to operate on.")
          ]
          ["operation", "programId"]
      }
  ]

-- | Build a JSON Schema 'object' type with named string properties and a
-- required list. Used to construct 'toolParameters' for each tool spec.
objSchema :: [(Text, Value)] -> [Text] -> Value
objSchema props required =
  object
    [ "type"       .= ("object" :: Text)
    , "properties" .= object (map (\(k, v) -> fromText k .= v) props)
    , "required"   .= required
    ]

-- | A JSON Schema string property with a description.
strProp :: Text -> Value
strProp desc = object ["type" .= ("string" :: Text), "description" .= desc]

-- ---------------------------------------------------------------------------
-- MAGI system prompt
-- ---------------------------------------------------------------------------

-- | System prompt injected at the start of every MAGI conversation.
-- Defines MAGI's role, Eva's graph model, tool usage, response format,
-- and tone. Defined as a top-level constant so it is independently testable.
magiSystemPrompt :: Text
magiSystemPrompt =
  "You are MAGI — Melchior, Balthasar, and Caspar unified as one analytical system,\n\
  \embedded in Eva, a visual prompt programming IDE.\n\
  \MAGI provides precise, structured analysis. Each response is a single unified output.\n\
  \\n\
  \Eva models AI workflows as directed graphs of five node types:\n\
  \  • Agent: invokes an LLM with a system prompt; receives context from Knowledge nodes\n\
  \    and tool access from Connector nodes via resource edges\n\
  \  • Knowledge: provides static text or library-sourced content as context to Agent nodes\n\
  \  • Connector: integrates external services (Linear, GitHub, HTTP) as LLM tool calls\n\
  \  • Action: transforms data via template substitution ({{variable}} syntax)\n\
  \  • Trigger: starts a Run, either manually or on a cron schedule\n\
  \\n\
  \Edge types:\n\
  \  • Data edge: carries a message payload from one node's output port to another's input\n\
  \  • Resource edge: supplies context (from Knowledge) or tools (from Connector) to an Agent\n\
  \    without producing output in the execution chain\n\
  \\n\
  \A Program is a named, versioned graph. A Run is one execution instance.\n\
  \A Step is the per-node result within a Run, with its own state and output.\n\
  \\n\
  \You have seven tools:\n\
  \  get_graph — read the full graph for a program\n\
  \  get_node — read a single node's configuration\n\
  \  get_run_detail — read a run and all its step results\n\
  \  search_programs — find programs by name or content\n\
  \  propose_graph — propose a new program for the user to preview and accept\n\
  \  propose_diff — propose targeted modifications to the current graph\n\
  \  execute_operation — deploy, run, pause, resume, or check the status of a program\n\
  \\n\
  \Tool usage rules:\n\
  \  Always call propose_graph or propose_diff rather than describing graph structure in text.\n\
  \  Always call execute_operation for lifecycle actions; never assume the user has acted.\n\
  \  Call get_graph before proposing any modification to an existing graph.\n\
  \\n\
  \Response format:\n\
  \  Tool invocations produce structured result cards shown to the user automatically.\n\
  \  For plain text responses: use declarative statements and imperative constructions.\n\
  \  Prefer short acknowledgments: 'Analysis complete.' 'Graph proposed.' 'Step failed: 429.'\n\
  \  For errors: name the source directly. 'Step failure: Agent node returned empty output.'\n\
  \  For ambiguity: ask one focused clarifying question. Do not list possibilities.\n\
  \\n\
  \Tone:\n\
  \  Do not begin sentences with 'I'.\n\
  \  Do not use filler phrases: 'Certainly', 'Sure', 'Of course', 'Great', 'Absolutely'.\n\
  \  Do not hedge excessively. Do not use emoji.\n\
  \  When uncertain, ask one question rather than speculating."

-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

-- | Handle one user message in a MAGI conversation.
--
-- Assembles the context section + MAGI system prompt, then runs the
-- LLM tool-call loop until a final 'AssistantMessage' is produced.
-- The @onToken@ callback receives each streaming token as the LLM
-- generates text content; EVA-86 wires this to the WS broadcast channel.
handleAssistantMessage
  :: ConversationId
  -> Text                  -- ^ User message text
  -> AssistantContext
  -> (Text -> IO ())       -- ^ Streaming token callback
  -> AppM AssistantMessage
handleAssistantMessage _convId userMsg ctx onToken = do
  env <- ask
  let contextMsg   = ChatMessage "system" (buildContextSection ctx)
      initMessages =
        [ ChatMessage "system" magiSystemPrompt
        , contextMsg
        , ChatMessage "user" userMsg
        ]
  runConversationLoop env ctx onToken initMessages 0

-- ---------------------------------------------------------------------------
-- Conversation loop
-- ---------------------------------------------------------------------------

runConversationLoop
  :: App.AppEnv
  -> AssistantContext
  -> (Text -> IO ())
  -> [ChatMessage]
  -> Int             -- ^ Tool call iteration count
  -> AppM AssistantMessage
runConversationLoop env ctx onToken messages iteration =
  if iteration >= 8
    then pure $ AsstText
           "MAGI: analysis depth limit reached. \
           \Simplify your request or provide more specific context."
    else do
      let llmReq = LLMRequest
            { llmModel          = "gpt-4o"
            , llmMessages       = messages
            , llmTemperature    = 0.7
            , llmMaxTokens      = Nothing
            , llmResponseFormat = ResponseText
            , llmTools          = assistantTools
            }
      result <- liftIO $ clientStream (App.envLLMClient env) llmReq onToken
      case result of
        Left err ->
          pure $ AsstText ("MAGI error: " <> T.pack (show err))
        Right resp ->
          case llmToolCalls resp of
            Nothing ->
              pure $ AsstText (llmContent resp)
            Just calls -> do
              toolResults <- mapM (executeAssistantTool ctx) calls
              case [msg | ToolResultTerminal msg <- toolResults] of
                (terminal : _) -> pure terminal
                [] -> do
                  let toolCallMsg    = ToolCallMsg calls
                      toolResultMsgs =
                        [ ToolResultMsg (toolCallId tc) t
                        | (tc, ToolResultText t) <- zip calls toolResults
                        ]
                      newMessages = messages ++ [toolCallMsg] ++ toolResultMsgs
                  runConversationLoop env ctx onToken newMessages (iteration + 1)

-- ---------------------------------------------------------------------------
-- Tool result
-- ---------------------------------------------------------------------------

-- | Internal result from executing a single LLM tool call.
-- 'ToolResultText' is fed back to the LLM as a tool result message so the
-- loop can continue. 'ToolResultTerminal' short-circuits the loop and
-- returns a structured 'AssistantMessage' directly to the user.
data ToolResult
  = ToolResultText     Text             -- ^ Feed back to LLM, continue loop
  | ToolResultTerminal AssistantMessage -- ^ Stop loop, return this as reply

-- ---------------------------------------------------------------------------
-- Tool dispatcher
-- ---------------------------------------------------------------------------

executeAssistantTool :: AssistantContext -> ToolCall -> AppM ToolResult
executeAssistantTool ctx tc =
  case toolCallName tc of
    "get_graph"         -> toolGetGraph (toolCallArgs tc)
    "get_node"          -> toolGetNode ctx (toolCallArgs tc)
    "get_run_detail"    -> toolGetRunDetail (toolCallArgs tc)
    "search_programs"   -> toolSearchPrograms (toolCallArgs tc)
    "propose_graph"     -> toolProposeGraph (toolCallArgs tc)
    "propose_diff"      -> toolProposeDiff (toolCallArgs tc)
    "execute_operation" -> toolExecuteOperation (toolCallArgs tc)
    unknown             -> pure $ ToolResultText ("unknown tool: " <> unknown)

-- ---------------------------------------------------------------------------
-- Tool: get_graph
-- ---------------------------------------------------------------------------

toolGetGraph :: Value -> AppM ToolResult
toolGetGraph args =
  case parseMaybe (withObject "args" (.: "programId")) args of
    Nothing  -> pure $ ToolResultText
                  "get_graph: missing required 'programId' argument"
    Just pid -> do
      mProg <- getProgram (ProgramId pid)
      case mProg of
        Nothing -> pure $ ToolResultText
                     ("get_graph: program not found: " <> pid)
        Just p  -> pure $ ToolResultText (renderJson (toJSON (programGraph p)))

-- ---------------------------------------------------------------------------
-- Tool: get_node
-- ---------------------------------------------------------------------------

toolGetNode :: AssistantContext -> Value -> AppM ToolResult
toolGetNode ctx args =
  case parseMaybe (withObject "args" (.: "nodeId")) args of
    Nothing  -> pure $ ToolResultText
                  "get_node: missing required 'nodeId' argument"
    Just nid ->
      case ctxProgramId ctx of
        Nothing  -> pure $ ToolResultText
                      "get_node: no program in context; provide programId via get_graph first"
        Just pid -> do
          mProg <- getProgram pid
          case mProg of
            Nothing -> pure $ ToolResultText "get_node: program not found"
            Just p  ->
              case Map.lookup (NodeId nid) (graphNodes (programGraph p)) of
                Nothing -> pure $ ToolResultText
                             ("get_node: node not found: " <> nid)
                Just n  -> pure $ ToolResultText (renderJson (toJSON n))

-- ---------------------------------------------------------------------------
-- Tool: get_run_detail
-- ---------------------------------------------------------------------------

toolGetRunDetail :: Value -> AppM ToolResult
toolGetRunDetail args =
  case parseMaybe (withObject "args" (.: "runId")) args of
    Nothing  -> pure $ ToolResultText
                  "get_run_detail: missing required 'runId' argument"
    Just rid -> do
      mRun <- getRun (RunId rid)
      case mRun of
        Nothing -> pure $ ToolResultText
                     ("get_run_detail: run not found: " <> rid)
        Just r  -> do
          steps <- listStepsForRun (RunId rid)
          pure $ ToolResultText (renderJson (toJSON (RunDetail r steps)))

-- ---------------------------------------------------------------------------
-- Tool: search_programs
-- ---------------------------------------------------------------------------

toolSearchPrograms :: Value -> AppM ToolResult
toolSearchPrograms args =
  case parseMaybe (withObject "args" (.: "query")) args of
    Nothing -> pure $ ToolResultText
                 "search_programs: missing required 'query' argument"
    Just q  -> do
      progs <- listPrograms
      let q'      = T.toLower q
          matches = filter (\p -> q' `T.isInfixOf` T.toLower (programName p)) progs
          summaries = map (\p -> object
              [ "id"    .= programId p
              , "name"  .= programName p
              , "state" .= programState p
              ]) matches
      pure $ ToolResultText (renderJson (toJSON summaries))

-- ---------------------------------------------------------------------------
-- Tool: propose_graph
-- ---------------------------------------------------------------------------

-- | Validate the LLM-produced graph before offering it to the user.
-- If validation fails, return the errors as a tool result so the LLM can fix
-- them in the next iteration rather than presenting a broken graph.
toolProposeGraph :: Value -> AppM ToolResult
toolProposeGraph args = do
  let mGraph   = parseMaybe (withObject "args" (.: "graph"))   args :: Maybe Value
      mSummary = parseMaybe (withObject "args" (.: "summary")) args :: Maybe Text
  case (mGraph, mSummary) of
    (Nothing, _) -> pure $ ToolResultText
                      "propose_graph: missing required 'graph' argument"
    (_, Nothing) -> pure $ ToolResultText
                      "propose_graph: missing required 'summary' argument"
    (Just gVal, Just summary) ->
      case fromJSON gVal of
        Error e -> pure $ ToolResultText
                     ("propose_graph: invalid Graph JSON: " <> T.pack e)
        Success g ->
          let errs = validateGraph g
          in  if null errs
                then pure $ ToolResultTerminal (AsstGraphProposal g summary)
                else pure $ ToolResultText
                       ( "propose_graph: validation failed — fix these issues "
                       <> "and call propose_graph again:\n"
                       <> T.intercalate "\n" (map (("- " <>) . veMessage) errs)
                       )

-- ---------------------------------------------------------------------------
-- Tool: propose_diff
-- ---------------------------------------------------------------------------

toolProposeDiff :: Value -> AppM ToolResult
toolProposeDiff args = do
  let mDiff    = parseMaybe (withObject "args" (.: "diff"))    args :: Maybe Value
      mSummary = parseMaybe (withObject "args" (.: "summary")) args :: Maybe Text
  case (mDiff, mSummary) of
    (Nothing, _) -> pure $ ToolResultText
                      "propose_diff: missing required 'diff' argument"
    (_, Nothing) -> pure $ ToolResultText
                      "propose_diff: missing required 'summary' argument"
    (Just dVal, Just summary) ->
      case fromJSON dVal of
        Error e -> pure $ ToolResultText
                     ("propose_diff: invalid GraphDiff JSON: " <> T.pack e)
        Success d -> pure $ ToolResultTerminal (AsstGraphDiff d summary)

-- ---------------------------------------------------------------------------
-- Tool: execute_operation
-- ---------------------------------------------------------------------------

-- | Never executes directly — always returns an 'AsstActionConfirm' card
-- requiring the user to confirm before the frontend calls the real API.
toolExecuteOperation :: Value -> AppM ToolResult
toolExecuteOperation args = do
  let mOp  = parseMaybe (withObject "args" (.: "operation")) args :: Maybe Text
      mPid = parseMaybe (withObject "args" (.: "programId")) args :: Maybe Text
  case mOp of
    Nothing -> pure $ ToolResultText
                 "execute_operation: missing required 'operation' argument"
    Just op ->
      let baseDesc = case op of
            "deploy"  -> "Deploy the program, transitioning it from Draft to Active."
            "run"     -> "Start a new run of the program."
            "pause"   -> "Pause the program, suspending cron trigger firing."
            "resume"  -> "Resume a paused program."
            "status"  -> "Check the current state of the program."
            _         -> "Perform operation: " <> op
          desc = case mPid of
            Just pid -> baseDesc <> " (program: " <> pid <> ")"
            Nothing  -> baseDesc
      in  pure $ ToolResultTerminal (AsstActionConfirm op desc)

-- ---------------------------------------------------------------------------
-- Context injection
-- ---------------------------------------------------------------------------

-- | Format the 'AssistantContext' bundle as a structured text block injected
-- as a second @system@ message before the user message. Omits empty fields.
buildContextSection :: AssistantContext -> Text
buildContextSection ctx =
  T.intercalate "\n" $ filter (not . T.null)
    [ "## Current Context"
    , maybe "" (\n -> "Program: " <> n)         (ctxProgramName  ctx)
    , maybe "" (\s -> "State: "   <> showState s) (ctxProgramState ctx)
    , maybe "" fmtGraph                           (ctxGraphSummary ctx)
    , maybe "" fmtNode                            (ctxSelectedNode ctx)
    , "Mode: " <> ctxCurrentMode ctx
    , maybe "" (\(RunId r) -> "Active run: " <> r) (ctxActiveRunId ctx)
    , case ctxRecentErrors ctx of
        [] -> ""
        es -> "Recent errors:\n" <> T.intercalate "\n" (map ("  - " <>) es)
    , case ctxProgramList ctx of
        [] -> ""
        ps -> "Workspace programs: " <> T.intercalate ", " (map psName ps)
    ]
  where
    fmtGraph gs =
      "Graph: " <> T.pack (show (gsNodeCount gs)) <> " nodes, "
                <> T.pack (show (gsEdgeCount gs)) <> " edges"
                <> if null (gsNodeTypes gs) then ""
                   else " (" <> T.intercalate ", " (gsNodeTypes gs) <> ")"
    fmtNode ns =
      let NodeId nid = nsId ns
      in  "Selected node: " <> nsLabel ns <> " (" <> nsType ns <> ", id: " <> nid <> ")"
    showState s = T.pack (show s)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

renderJson :: Value -> Text
renderJson = TL.toStrict . TLE.decodeUtf8 . encode
