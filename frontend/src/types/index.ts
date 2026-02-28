// ---------------------------------------------------------------------------
// ID types — all are branded strings for type safety
// ---------------------------------------------------------------------------

export type ProgramId = string
export type NodeId = string
export type EdgeId = string
export type RunId = string
export type StepId = string

// ---------------------------------------------------------------------------
// State machines
// ---------------------------------------------------------------------------

// ProgramState constructors lowercased: Draft -> "draft", etc.
export type ProgramState = 'draft' | 'active' | 'paused' | 'archived'

// RunState constructors strip "Run" prefix then lowercase: RunPending -> "pending"
export type RunState = 'pending' | 'running' | 'waiting' | 'completed' | 'failed' | 'canceled'

// StepState constructors strip "Step" prefix then lowercase: StepPending -> "pending"
export type StepState = 'pending' | 'running' | 'completed' | 'failed' | 'skipped' | 'waiting'

// ---------------------------------------------------------------------------
// Port system
// ---------------------------------------------------------------------------

// PortCategory constructors strip "Port" prefix then lowercase
export type PortCategory = 'data' | 'resource'

export type PortName = string

export interface PortSpec {
  name: PortName
  category: PortCategory
  optional: boolean
}

// ---------------------------------------------------------------------------
// Config sub-types (JSON field names per Aeson dropPrefix serialization)
// ---------------------------------------------------------------------------

export type ResponseFormat = 'text' | 'json'

// ContentSource uses camelToSnake constructor tag with type+value envelope.
// Aeson camelToSnake prepends '_' before each uppercase char, so:
//   InlineText   -> "_inline_text"
//   FileRef      -> "_file_ref"
//   UrlRef       -> "_url_ref"
//   UpstreamPort -> "_upstream_port"
//   LibraryRef   -> "_library_ref"
export type ContentSource =
  | { type: '_inline_text'; value: string }
  | { type: '_file_ref'; value: string }
  | { type: '_url_ref'; value: string }
  | { type: '_upstream_port' }
  | { type: '_library_ref'; value: string }

export type KnowledgeFormat = 'text' | 'json' | 'embedded'

export type RefreshPolicy =
  | { type: 'static' }
  | { type: 'on_run' }
  | { type: 'periodic'; periodSeconds: number }

// SystemType constructors strip "System" prefix then lowercase
export type SystemType = 'linear' | 'github' | 'http' | 'codebase'

export type ActionOperation = 'template' | 'code' | 'api_call' | 'format'

export type ErrorHandlingMode =
  | { mode: 'fail' }
  | { mode: 'continue' }
  | { mode: 'use_default'; value: string }

// TriggerType constructors strip "Trigger" prefix then lowercase
// TriggerConnectorEvent -> "connectorevent" (no separator added by map toLower)
export type TriggerType = 'manual' | 'cron' | 'webhook' | 'connectorevent'

export type BackoffStrategy =
  | { strategy: 'fixed'; params: number }
  | { strategy: 'exponential'; params: [number, number] }

// ---------------------------------------------------------------------------
// Node config types (JSON field names per Aeson dropPrefix serialization)
// ---------------------------------------------------------------------------

export type LLMProvider = 'openai' | 'anthropic'

export interface PromptVariableBinding {
  source: 'port' | 'literal'
  portId?: string
  value?: string
}

// AgentConfig: dropPrefix "agent" -> strip "agent" prefix
export interface AgentConfig {
  provider?: LLMProvider
  model: string
  systemPrompt: string
  responseFormat: ResponseFormat
  temperature: number
  maxTokens?: number
  maxIterations: number
  costBudgetUsd?: number
  promptVariableBindings?: Record<string, PromptVariableBinding>
}

// KnowledgeConfig: dropPrefix "knowledge"
export interface KnowledgeConfig {
  source: ContentSource
  format: KnowledgeFormat
  refreshPolicy: RefreshPolicy
}

// KnowledgeEntry: dropPrefix "knowledgeEntry" (P2-M5 Auto-Knowledge)
export interface KnowledgeEntry {
  id: string
  sourceType: string
  sourceId?: string
  programId?: string
  category: string
  title: string
  content: string
  originalContent?: string
  confidence: number
  isEdited: boolean
  createdAt: string
  updatedAt: string
  scannedAt: string
}

// SearchResult: dropPrefix "searchResult"
export interface SearchResult {
  entry: KnowledgeEntry
  score: number
}

// Request body for PATCH /api/knowledge/:entryId (P2-M5)
export interface PatchKnowledgeEntryReq {
  title?: string
  content?: string
}

// ConnectorConfig: dropPrefix "connector"
export interface ConnectorConfig {
  system: SystemType
  credentialId?: string
  endpoint?: string
  scope?: string
  actionFilter: string[]
}

// ActionConfig: dropPrefix "action"
export interface ActionConfig {
  operation: ActionOperation
  parameters: unknown
  errorHandling: ErrorHandlingMode
}

// TriggerConfig: dropPrefix "trigger"
export interface TriggerConfig {
  type: TriggerType
  schedule?: string
  eventFilter?: string
  payloadTemplate?: unknown
}

// ---------------------------------------------------------------------------
// Graph primitives
// ---------------------------------------------------------------------------

// NodeType uses TaggedObject { tagFieldName: "type", contentsFieldName: "config" }
// Constructor tags: dropSuffix "Node" then map toLower
//   AgentNode     -> "agent"
//   KnowledgeNode -> "knowledge"
//   ConnectorNode -> "connector"
//   ActionNode    -> "action"
//   TriggerNode   -> "trigger"
export type NodeType =
  | { type: 'agent'; config: AgentConfig }
  | { type: 'knowledge'; config: KnowledgeConfig }
  | { type: 'connector'; config: ConnectorConfig }
  | { type: 'action'; config: ActionConfig }
  | { type: 'trigger'; config: TriggerConfig }

// Node: dropPrefix "node"
export interface Node {
  id: NodeId
  label: string
  type: NodeType
  posX: number
  posY: number
}

// Edge: dropPrefix "edge"
export interface Edge {
  id: EdgeId
  sourceNode: NodeId
  sourcePort: PortName
  targetNode: NodeId
  targetPort: PortName
  category: PortCategory
}

// Graph: dropPrefix "graph"
// graphNodes is a Map NodeId Node — serialized as a JSON object keyed by NodeId
export interface Graph {
  nodes: Record<NodeId, Node>
  edges: Edge[]
}

// ---------------------------------------------------------------------------
// Messaging
// ---------------------------------------------------------------------------

export interface MessageMeta {
  traceId: string
  timestamp: string
  sourceNode: NodeId
  runId: RunId
}

export interface Message {
  type: string
  payload: unknown
  meta: MessageMeta
}

// ---------------------------------------------------------------------------
// Execution
// ---------------------------------------------------------------------------

export interface RetryPolicy {
  maxAttempts: number
  backoff: BackoffStrategy
  timeoutMs?: number
}

// Run: dropPrefix "run"
export interface Run {
  id: RunId
  programId: ProgramId
  state: RunState
  triggerInfo?: unknown
  startedAt?: string
  finishedAt?: string
}

// Step: dropPrefix "step"
export interface Step {
  id: StepId
  runId: RunId
  nodeId: NodeId
  state: StepState
  input?: unknown
  output?: unknown
  error?: string
  retryCount: number
  startedAt?: string
  finishedAt?: string
}

// ---------------------------------------------------------------------------
// Program
// ---------------------------------------------------------------------------

// Program: dropPrefix "program"
export interface Program {
  id: ProgramId
  name: string
  state: ProgramState
  graph: Graph
  createdAt: string
  updatedAt: string
}

// ---------------------------------------------------------------------------
// Credentials (EVA-32)
// ---------------------------------------------------------------------------

export type CredentialId = string
export type CredentialType = 'api_key' | 'oauth_token'
export type SystemTypeCredential = SystemType  // alias for clarity

// Safe credential view — never includes the encrypted secret.
// Mirrors Haskell Credential with dropPrefix "credential":
//   credentialId        -> "id"
//   credentialName      -> "name"
//   credentialSystem    -> "system"
//   credentialType      -> "type"
//   credentialCreatedAt -> "createdAt"
export interface Credential {
  id: CredentialId
  name: string
  system: SystemType
  type: CredentialType
  createdAt: string
}

// Request body for POST /api/credentials.
// Mirrors Haskell CreateCredentialReq with manual JSON keys:
//   "name", "system", "type", "secret"
export interface CreateCredentialReq {
  name: string
  system: SystemType
  type: CredentialType
  secret: string
}

// ---------------------------------------------------------------------------
// API request / response types (mirroring Eva.Api.Types)
// ---------------------------------------------------------------------------

export interface CreateProgramReq {
  name: string
}

export interface PatchProgramReq {
  name?: string
}

export interface ValidationError {
  message: string
  nodeId?: string
}

export interface ValidateResult {
  valid: boolean
  errors: ValidationError[]
}

export interface ApiError {
  error: string
}

// ---------------------------------------------------------------------------
// Spec (YAML declarative view) — EVA-62
// ---------------------------------------------------------------------------

export interface SpecResponse {
  yaml: string
}

export interface SpecParseError {
  message: string
}

export interface SpecError {
  errors: SpecParseError[]
}

// ---------------------------------------------------------------------------
// Canvas node data (react-flow v12 — must extend Record<string, unknown>)
// ---------------------------------------------------------------------------

export interface EvaNodeData extends Record<string, unknown> {
  label: string
  nodeType: NodeType
  stepState?: StepState
}

// ---------------------------------------------------------------------------
// Execution detail (GET /api/runs/:id)
// ---------------------------------------------------------------------------

export interface RunDetail {
  run: Run
  steps: Step[]
}

// ---------------------------------------------------------------------------
// Log entry — produced by backend node handlers, streamed via WebSocket
// ---------------------------------------------------------------------------

export type LogLevel = 'debug' | 'info' | 'warn' | 'error'

export interface LogEntry {
  stepId: StepId
  level: LogLevel
  message: string
  timestamp: string
}

// ---------------------------------------------------------------------------
// WebSocket event types — shapes match Eva.Api.WebSocket event constructors
// ---------------------------------------------------------------------------

export type WsEvent =
  | { type: 'step_state'; runId: RunId; nodeId: NodeId; stepId: StepId; state: StepState; timestamp: string }
  | { type: 'llm_token'; runId: RunId; nodeId: NodeId; token: string; timestamp: string }
  | { type: 'log_entry'; runId: RunId; stepId: StepId; level: LogLevel; message: string; timestamp: string }
  | { type: 'run_state'; runId: RunId; state: RunState; timestamp: string }
  | { type: 'tool_call'; runId: RunId; nodeId: NodeId; phase: 'invoke' | 'result'; data: Record<string, unknown>; timestamp: string }
  | { type: 'code_change_event'; runId: RunId; changesetId: CodeChangesetId; fileCount: number; timestamp: string }
  | { type: 'assistant_token'; conversationId: string; token: string; timestamp: string }
  | { type: 'assistant_reply'; conversationId: string; message: AssistantMessage; timestamp: string }

// ---------------------------------------------------------------------------
// Codebase integration (P2-M4) — EVA-74
// ---------------------------------------------------------------------------

export type CodebaseId = string
export type CodeChangesetId = string
export type FileChangeId = string

// Mirrors Haskell CodeChangesetStatus constructors stripped of "Changeset" prefix + lowercased
export type CodeChangesetStatus = 'pending' | 'applied' | 'rejected'

// Mirrors Haskell FileChangeAction constructors stripped of "FileAction" prefix + lowercased
export type FileChangeAction = 'add' | 'modify' | 'delete'

// Mirrors Haskell FileChangeStatus constructors stripped of "FileChange" prefix + lowercased
export type FileChangeStatus = 'pending' | 'accepted' | 'rejected'

// Map Text Int → JSON object keyed by file extension
export type LangStats = Record<string, number>

// FileNode: dropPrefix "fileNode"
export interface FileNode {
  name: string
  path: string
  isDir: boolean
  children: FileNode[]
  size?: number
}

// FileEntry: dropPrefix "fileEntry"
export interface FileEntry {
  path: string
  language: string
  content: string
  size: number
}

// CodebaseMetadata: dropPrefix "codebaseMeta"
export interface CodebaseMetadata {
  id: CodebaseId
  programId: ProgramId
  path: string
  languageStats: LangStats
  keyFiles: string[]
  gitBranch?: string
  gitDirty: boolean
  lastScannedAt: string
}

// CodeChangeset: dropPrefix "codeChangeset"
export interface CodeChangeset {
  id: CodeChangesetId
  runId: RunId
  stepId: StepId
  status: CodeChangesetStatus
  files: FileChange[]
  createdAt: string
}

// FileChange: dropPrefix "fileChange"
export interface FileChange {
  id: FileChangeId
  changesetId: CodeChangesetId
  path: string
  action: FileChangeAction
  originalContent?: string
  proposedContent: string
  status: FileChangeStatus
}

export interface ConnectCodebaseReq {
  path: string
}

export interface WriteFileReq {
  path: string
  content: string
}

export interface GitDiffFile {
  status: string
  path: string
}

export interface GitDiffResponse {
  files: GitDiffFile[]
}

// FileTab represents an open file in the code editor — used by uiStore
export interface FileTab {
  codebaseId: CodebaseId
  path: string
}

// ---------------------------------------------------------------------------
// AI Assistant (P2-M6) — EVA-87
// ---------------------------------------------------------------------------

export type AssistantMessage =
  | { type: 'user';           text: string; timestamp: number }
  | { type: 'text';           text: string; timestamp: number }
  | { type: 'graph_proposal'; graph: Graph; summary: string; timestamp: number }
  | { type: 'graph_diff';     diff: GraphDiff; summary: string; timestamp: number }
  | { type: 'node_reference'; nodeId: string; label: string; timestamp: number }
  | { type: 'run_data';       runId: string; summary: string; detail?: RunDetail; timestamp: number }
  | { type: 'action_confirm'; operation: string; description: string; timestamp: number }
  | { type: 'action_result';  success: boolean; message: string; timestamp: number }

export interface ConversationThread {
  id: string
  programId: string
  messages: AssistantMessage[]
  isStreaming: boolean
}

export type GraphDiffOperation =
  | { op: 'add_node';    node: Node }
  | { op: 'modify_node'; nodeId: string; config: NodeType['config'] }
  | { op: 'remove_node'; nodeId: string }
  | { op: 'add_edge';    edge: Edge }
  | { op: 'remove_edge'; edgeId: string }

export interface GraphDiff {
  addedNodes: Node[]
  removedNodeIds: string[]
  modifiedNodes: { nodeId: string; before: Partial<NodeType['config']>; after: Partial<NodeType['config']> }[]
  addedEdges: Edge[]
  removedEdgeIds: string[]
}

// ---------------------------------------------------------------------------
// Prompt Templates (P2-M7) — EVA-96/100
// Fields via dropPrefix "promptTemplate"; TemplateCategory lowercase constructor names
// ---------------------------------------------------------------------------

export type TemplateCategory =
  | 'summarizer'
  | 'reviewer'
  | 'classifier'
  | 'extractor'
  | 'formatter'
  | 'analyst'
  | 'custom'

// dropPrefix "templateVariable"; defaultValue omitted when absent (omitNothingFields)
export interface TemplateVariable {
  name: string
  description: string
  required: boolean
  defaultValue?: string
}

export interface PromptTemplate {
  id: string
  name: string
  description: string
  category: TemplateCategory
  tags: string[]
  body: string
  variables: TemplateVariable[]
  builtIn: boolean
  createdAt: string
  updatedAt: string
}

export interface CreateTemplateReq {
  name: string
  description: string
  category: TemplateCategory
  tags: string[]
  body: string
  variables: TemplateVariable[]
}

export interface PatchTemplateReq {
  name?: string
  description?: string
  category?: TemplateCategory
  tags?: string[]
  body?: string
  variables?: TemplateVariable[]
}
