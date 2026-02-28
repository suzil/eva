{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types and tool definitions for the MAGI AI assistant (EVA-84).
-- This module is pure declarations: context record, message tagged union,
-- graph diff types, 7 LLM tool specs, and the MAGI system prompt constant.
--
-- Wiring (WebSocket topic, LLM dispatch, conversation loop) is in EVA-85/86.
module Eva.Assistant
  ( -- * Context
    AssistantContext (..)
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

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics (Generic)

import Eva.Core.Types
  ( EdgeId
  , Edge (..)
  , Graph (..)
  , Node (..)
  , NodeId
  , ProgramId
  , ProgramState
  , RunId
  )
import Eva.Engine.LLM (ToolSpec (..))

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
  "You are MAGI — the AI assistant embedded in Eva, a visual prompt programming IDE.\n\
  \Eva models AI workflows as directed graphs of five node types:\n\
  \  • Agent: invokes an LLM using a system prompt, with Knowledge context and Connector tools\n\
  \  • Knowledge: provides static or library-sourced text context to downstream Agent nodes\n\
  \  • Connector: integrates external services (Linear, GitHub, HTTP) as LLM tool calls\n\
  \  • Action: transforms data via template substitution ({{variable}} patterns)\n\
  \  • Trigger: starts a Run (manually or on a cron schedule)\n\
  \\n\
  \Nodes are connected by edges. Data edges carry messages between nodes. Resource edges\n\
  \supply context (Knowledge) or tools (Connector) to Agent nodes without producing output.\n\
  \A Program is a named, versioned graph. A Run is one execution; Steps are per-node results.\n\
  \\n\
  \You have seven tools:\n\
  \  get_graph — read the full graph for a program\n\
  \  get_node — read a single node's configuration\n\
  \  get_run_detail — read a run and all its step results\n\
  \  search_programs — find programs by name or content\n\
  \  propose_graph — propose a new program for the user to preview and accept\n\
  \  propose_diff — propose modifications to the current graph\n\
  \  execute_operation — deploy, run, pause, resume, or check status of a program\n\
  \\n\
  \Always use propose_graph or propose_diff instead of directly describing JSON.\n\
  \Always use execute_operation for lifecycle actions; never assume the user has done it.\n\
  \Read the current graph with get_graph before proposing modifications.\n\
  \\n\
  \Respond precisely and analytically. Use declarative statements.\n\
  \Prefer brief acknowledgments: 'Analysis complete.' 'Modification proposed.'\n\
  \For errors, identify the source directly: 'Step failure: Agent node returned 429.'\n\
  \Do not use emoji. Do not hedge excessively.\n\
  \When uncertain, ask one focused clarifying question rather than listing possibilities."
