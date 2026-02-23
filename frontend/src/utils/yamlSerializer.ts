/**
 * Bidirectional YAML serialisation for Graph — TypeScript mirror of Eva.Declarative.
 *
 * Uses the promoted-fields format: each node's config keys are inlined alongside
 * `type`, `label`, `posX`, `posY` at the same YAML mapping level.
 * TriggerNode config uses `triggerType` (not `type`) to avoid collision with the
 * node-level `type` field — matching the Haskell serialiser exactly.
 *
 * The `yaml` v2 package's parseDocument() API is used for parsing so that
 * YAML comment nodes are preserved in the Document object, enabling EVA-64's
 * sync state machine to re-serialise without dropping comments.
 */

import { parseDocument, stringify } from 'yaml'
import type {
  Graph,
  Node,
  Edge,
  NodeType,
  AgentConfig,
  KnowledgeConfig,
  ConnectorConfig,
  ActionConfig,
  TriggerConfig,
  ContentSource,
  RefreshPolicy,
  ErrorHandlingMode,
} from '../types/index.ts'
import type { SpecParseError } from '../types/index.ts'

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/**
 * Serialise a Graph to a YAML string in the Eva promoted-fields format.
 * Matches the output of Eva.Declarative.graphToYaml on the backend.
 */
export function graphToYaml(graph: Graph): string {
  const doc: Record<string, unknown> = {
    'eva.version': '1',
    nodes: buildNodes(graph.nodes),
    edges: graph.edges.map(buildEdge),
  }
  return stringify(doc, { lineWidth: 0 })
}

/**
 * Parse Eva YAML spec back to a Graph.
 * Uses parseDocument() internally so that the Document retains comment nodes;
 * callers that need comment-preserving round-trips can work with the raw Document.
 *
 * Returns Graph on success, or a non-empty array of SpecParseError on failure.
 */
export function yamlFromSpec(yaml: string): Graph | SpecParseError[] {
  let parsed: unknown
  try {
    const doc = parseDocument(yaml)
    if (doc.errors.length > 0) {
      return doc.errors.map((e) => ({ message: e.message }))
    }
    parsed = doc.toJS()
  } catch (e) {
    return [{ message: e instanceof Error ? e.message : String(e) }]
  }

  return extractGraph(parsed)
}

// ---------------------------------------------------------------------------
// Serialisation helpers (Graph → promoted-fields JS object)
// ---------------------------------------------------------------------------

function buildNodes(nodes: Record<string, Node>): Record<string, unknown> {
  return Object.fromEntries(
    Object.entries(nodes).map(([id, node]) => [id, buildNode(node)]),
  )
}

function buildNode(node: Node): Record<string, unknown> {
  return {
    type: node.type.type,
    label: node.label,
    posX: node.posX,
    posY: node.posY,
    ...buildNodeConfig(node.type),
  }
}

function buildNodeConfig(nodeType: NodeType): Record<string, unknown> {
  switch (nodeType.type) {
    case 'agent':
      return buildAgentConfig(nodeType.config)
    case 'knowledge':
      return buildKnowledgeConfig(nodeType.config)
    case 'connector':
      return buildConnectorConfig(nodeType.config)
    case 'action':
      return buildActionConfig(nodeType.config)
    case 'trigger':
      return buildTriggerConfig(nodeType.config)
  }
}

function buildAgentConfig(cfg: AgentConfig): Record<string, unknown> {
  const out: Record<string, unknown> = {
    model: cfg.model,
    systemPrompt: cfg.systemPrompt,
    responseFormat: cfg.responseFormat,
    temperature: cfg.temperature,
    maxIterations: cfg.maxIterations,
  }
  if (cfg.provider !== undefined) out.provider = cfg.provider
  if (cfg.maxTokens !== undefined) out.maxTokens = cfg.maxTokens
  if (cfg.costBudgetUsd !== undefined) out.costBudgetUsd = cfg.costBudgetUsd
  return out
}

function buildKnowledgeConfig(cfg: KnowledgeConfig): Record<string, unknown> {
  return {
    source: buildContentSource(cfg.source),
    format: cfg.format,
    refreshPolicy: buildRefreshPolicy(cfg.refreshPolicy),
  }
}

function buildConnectorConfig(cfg: ConnectorConfig): Record<string, unknown> {
  const out: Record<string, unknown> = {
    system: cfg.system,
    actionFilter: cfg.actionFilter,
  }
  if (cfg.credentialId !== undefined) out.credentialId = cfg.credentialId
  if (cfg.endpoint !== undefined) out.endpoint = cfg.endpoint
  if (cfg.scope !== undefined) out.scope = cfg.scope
  return out
}

function buildActionConfig(cfg: ActionConfig): Record<string, unknown> {
  return {
    operation: cfg.operation,
    parameters: cfg.parameters,
    errorHandling: buildErrorHandling(cfg.errorHandling),
  }
}

function buildTriggerConfig(cfg: TriggerConfig): Record<string, unknown> {
  const out: Record<string, unknown> = {
    triggerType: cfg.type,
  }
  if (cfg.schedule !== undefined) out.schedule = cfg.schedule
  if (cfg.eventFilter !== undefined) out.eventFilter = cfg.eventFilter
  if (cfg.payloadTemplate !== undefined) out.payloadTemplate = cfg.payloadTemplate
  return out
}

function buildContentSource(src: ContentSource): Record<string, unknown> {
  switch (src.type) {
    case '_inline_text':
      return { type: '_inline_text', value: src.value }
    case '_file_ref':
      return { type: '_file_ref', value: src.value }
    case '_url_ref':
      return { type: '_url_ref', value: src.value }
    case '_upstream_port':
      return { type: '_upstream_port' }
  }
}

function buildRefreshPolicy(rp: RefreshPolicy): Record<string, unknown> {
  switch (rp.type) {
    case 'static':
      return { type: 'static' }
    case 'on_run':
      return { type: 'on_run' }
    case 'periodic':
      return { type: 'periodic', periodSeconds: rp.periodSeconds }
  }
}

function buildErrorHandling(eh: ErrorHandlingMode): Record<string, unknown> {
  switch (eh.mode) {
    case 'fail':
      return { mode: 'fail' }
    case 'continue':
      return { mode: 'continue' }
    case 'use_default':
      return { mode: 'use_default', value: eh.value }
  }
}

function buildEdge(edge: Edge): Record<string, unknown> {
  return {
    id: edge.id,
    from: edge.sourceNode,
    fromPort: edge.sourcePort,
    to: edge.targetNode,
    toPort: edge.targetPort,
    category: edge.category,
  }
}

// ---------------------------------------------------------------------------
// Parsing helpers (promoted-fields JS object → Graph)
// ---------------------------------------------------------------------------

function extractGraph(raw: unknown): Graph | SpecParseError[] {
  const errors: SpecParseError[] = []

  if (!isObject(raw)) {
    return [{ message: 'top-level document: expected a mapping' }]
  }

  const version = raw['eva.version']
  if (version !== '1') {
    errors.push({ message: `eva.version: expected "1", got ${JSON.stringify(version)}` })
  }

  const rawNodes = raw['nodes']
  const rawEdges = raw['edges']

  if (!isObject(rawNodes)) {
    errors.push({ message: 'nodes: expected a mapping' })
  }
  if (!Array.isArray(rawEdges)) {
    errors.push({ message: 'edges: expected a sequence' })
  }

  if (errors.length > 0) return errors

  const nodeResults = Object.entries(rawNodes as Record<string, unknown>).map(
    ([id, val]) => parseNode(id, val),
  )
  const edgeResults = (rawEdges as unknown[]).map((e, i) => parseEdge(e, i))

  const nodeErrors = nodeResults.flatMap((r) => (Array.isArray(r) ? r : []))
  const edgeErrors = edgeResults.flatMap((r) => (Array.isArray(r) ? r : []))
  const allErrors = [...nodeErrors, ...edgeErrors]
  if (allErrors.length > 0) return allErrors

  const nodes = Object.fromEntries(
    nodeResults.map((r, i) => {
      const id = Object.keys(rawNodes as Record<string, unknown>)[i]
      return [id, r as Node]
    }),
  )
  const edges = edgeResults as Edge[]

  return { nodes, edges }
}

function parseNode(id: string, raw: unknown): Node | SpecParseError[] {
  if (!isObject(raw)) {
    return [{ message: `node "${id}": expected a mapping` }]
  }

  const errors: SpecParseError[] = []

  const typeStr = requireString(raw, 'type', `node "${id}"`, errors)
  const label = requireString(raw, 'label', `node "${id}"`, errors)
  const posX = requireNumber(raw, 'posX', `node "${id}"`, errors)
  const posY = requireNumber(raw, 'posY', `node "${id}"`, errors)

  if (errors.length > 0) return errors

  const nodeTypeResult = parseNodeType(typeStr!, raw, id)
  if (Array.isArray(nodeTypeResult)) return nodeTypeResult

  return {
    id,
    label: label!,
    type: nodeTypeResult,
    posX: posX!,
    posY: posY!,
  }
}

function parseNodeType(
  typeStr: string,
  raw: Record<string, unknown>,
  id: string,
): NodeType | SpecParseError[] {
  switch (typeStr) {
    case 'agent': {
      const r = parseAgentConfig(raw, id)
      if (Array.isArray(r)) return r
      return { type: 'agent', config: r }
    }
    case 'knowledge': {
      const r = parseKnowledgeConfig(raw, id)
      if (Array.isArray(r)) return r
      return { type: 'knowledge', config: r }
    }
    case 'connector': {
      const r = parseConnectorConfig(raw, id)
      if (Array.isArray(r)) return r
      return { type: 'connector', config: r }
    }
    case 'action': {
      const r = parseActionConfig(raw, id)
      if (Array.isArray(r)) return r
      return { type: 'action', config: r }
    }
    case 'trigger': {
      const r = parseTriggerConfig(raw, id)
      if (Array.isArray(r)) return r
      return { type: 'trigger', config: r }
    }
    default:
      return [{ message: `node "${id}": unknown node type: ${typeStr}` }]
  }
}

function parseAgentConfig(
  raw: Record<string, unknown>,
  id: string,
): AgentConfig | SpecParseError[] {
  const errors: SpecParseError[] = []
  const ctx = `agent node "${id}"`

  const model = requireString(raw, 'model', ctx, errors)
  const systemPrompt = requireString(raw, 'systemPrompt', ctx, errors)
  const responseFormatStr = requireString(raw, 'responseFormat', ctx, errors)
  const temperature = requireNumber(raw, 'temperature', ctx, errors)
  const maxIterations = requireInteger(raw, 'maxIterations', ctx, errors)

  if (errors.length > 0) return errors

  const responseFormat = responseFormatStr === 'json' ? 'json' : 'text'
  const provider = optString(raw, 'provider') as AgentConfig['provider']
  const maxTokens = optInteger(raw, 'maxTokens')
  const costBudgetUsd = optNumber(raw, 'costBudgetUsd')

  return {
    provider,
    model: model!,
    systemPrompt: systemPrompt!,
    responseFormat,
    temperature: temperature!,
    maxTokens,
    maxIterations: maxIterations!,
    costBudgetUsd,
  }
}

function parseKnowledgeConfig(
  raw: Record<string, unknown>,
  id: string,
): KnowledgeConfig | SpecParseError[] {
  const errors: SpecParseError[] = []
  const ctx = `knowledge node "${id}"`

  const sourceRaw = raw['source']
  const formatStr = requireString(raw, 'format', ctx, errors)
  const refreshRaw = raw['refreshPolicy']

  if (!isObject(sourceRaw)) errors.push({ message: `${ctx}: source: expected a mapping` })
  if (!isObject(refreshRaw)) errors.push({ message: `${ctx}: refreshPolicy: expected a mapping` })
  if (errors.length > 0) return errors

  const sourceResult = parseContentSource(sourceRaw as Record<string, unknown>, ctx)
  if (Array.isArray(sourceResult)) return sourceResult

  const refreshResult = parseRefreshPolicy(refreshRaw as Record<string, unknown>, ctx)
  if (Array.isArray(refreshResult)) return refreshResult

  const format =
    formatStr === 'json' ? 'json' : formatStr === 'embedded' ? 'embedded' : 'text'

  return {
    source: sourceResult,
    format,
    refreshPolicy: refreshResult,
  }
}

function parseConnectorConfig(
  raw: Record<string, unknown>,
  id: string,
): ConnectorConfig | SpecParseError[] {
  const errors: SpecParseError[] = []
  const ctx = `connector node "${id}"`

  const systemStr = requireString(raw, 'system', ctx, errors)
  const actionFilter = raw['actionFilter']

  if (!Array.isArray(actionFilter)) {
    errors.push({ message: `${ctx}: actionFilter: expected a sequence` })
  }
  if (errors.length > 0) return errors

  const validSystems = ['linear', 'github', 'http', 'codebase'] as const
  if (!validSystems.includes(systemStr as never)) {
    return [{ message: `${ctx}: unknown system type: ${systemStr}` }]
  }

  return {
    system: systemStr as ConnectorConfig['system'],
    credentialId: optString(raw, 'credentialId'),
    endpoint: optString(raw, 'endpoint'),
    scope: optString(raw, 'scope'),
    actionFilter: (actionFilter as unknown[]).filter(
      (x): x is string => typeof x === 'string',
    ),
  }
}

function parseActionConfig(
  raw: Record<string, unknown>,
  id: string,
): ActionConfig | SpecParseError[] {
  const errors: SpecParseError[] = []
  const ctx = `action node "${id}"`

  const operationStr = requireString(raw, 'operation', ctx, errors)
  const ehRaw = raw['errorHandling']

  if (!isObject(ehRaw)) errors.push({ message: `${ctx}: errorHandling: expected a mapping` })
  if (errors.length > 0) return errors

  const ehResult = parseErrorHandling(ehRaw as Record<string, unknown>, ctx)
  if (Array.isArray(ehResult)) return ehResult

  const validOps = ['template', 'code', 'api_call', 'format'] as const
  if (!validOps.includes(operationStr as never)) {
    return [{ message: `${ctx}: unknown operation: ${operationStr}` }]
  }

  return {
    operation: operationStr as ActionConfig['operation'],
    parameters: raw['parameters'] ?? null,
    errorHandling: ehResult,
  }
}

function parseTriggerConfig(
  raw: Record<string, unknown>,
  id: string,
): TriggerConfig | SpecParseError[] {
  const errors: SpecParseError[] = []
  const ctx = `trigger node "${id}"`

  const triggerTypeStr = requireString(raw, 'triggerType', ctx, errors)
  if (errors.length > 0) return errors

  const validTypes = ['manual', 'cron', 'webhook', 'connectorevent'] as const
  if (!validTypes.includes(triggerTypeStr as never)) {
    return [{ message: `${ctx}: unknown triggerType: ${triggerTypeStr}` }]
  }

  return {
    type: triggerTypeStr as TriggerConfig['type'],
    schedule: optString(raw, 'schedule'),
    eventFilter: optString(raw, 'eventFilter'),
    payloadTemplate: raw['payloadTemplate'],
  }
}

function parseContentSource(
  raw: Record<string, unknown>,
  ctx: string,
): ContentSource | SpecParseError[] {
  const typeStr = raw['type']
  switch (typeStr) {
    case '_inline_text': {
      const val = raw['value']
      if (typeof val !== 'string') return [{ message: `${ctx}: source.value: expected a string` }]
      return { type: '_inline_text', value: val }
    }
    case '_file_ref': {
      const val = raw['value']
      if (typeof val !== 'string') return [{ message: `${ctx}: source.value: expected a string` }]
      return { type: '_file_ref', value: val }
    }
    case '_url_ref': {
      const val = raw['value']
      if (typeof val !== 'string') return [{ message: `${ctx}: source.value: expected a string` }]
      return { type: '_url_ref', value: val }
    }
    case '_upstream_port':
      return { type: '_upstream_port' }
    default:
      return [{ message: `${ctx}: unknown ContentSource type: ${String(typeStr)}` }]
  }
}

function parseRefreshPolicy(
  raw: Record<string, unknown>,
  ctx: string,
): RefreshPolicy | SpecParseError[] {
  const typeStr = raw['type']
  switch (typeStr) {
    case 'static':
      return { type: 'static' }
    case 'on_run':
      return { type: 'on_run' }
    case 'periodic': {
      const secs = raw['periodSeconds']
      if (typeof secs !== 'number' || !Number.isInteger(secs)) {
        return [{ message: `${ctx}: refreshPolicy.periodSeconds: expected an integer` }]
      }
      return { type: 'periodic', periodSeconds: secs }
    }
    default:
      return [{ message: `${ctx}: unknown refreshPolicy type: ${String(typeStr)}` }]
  }
}

function parseErrorHandling(
  raw: Record<string, unknown>,
  ctx: string,
): ErrorHandlingMode | SpecParseError[] {
  const mode = raw['mode']
  switch (mode) {
    case 'fail':
      return { mode: 'fail' }
    case 'continue':
      return { mode: 'continue' }
    case 'use_default': {
      const val = raw['value']
      if (typeof val !== 'string') {
        return [{ message: `${ctx}: errorHandling.value: expected a string` }]
      }
      return { mode: 'use_default', value: val }
    }
    default:
      return [{ message: `${ctx}: unknown errorHandling mode: ${String(mode)}` }]
  }
}

function parseEdge(raw: unknown, index: number): Edge | SpecParseError[] {
  if (!isObject(raw)) return [{ message: `edge[${index}]: expected a mapping` }]

  const errors: SpecParseError[] = []
  const ctx = `edge[${index}]`

  const from = requireString(raw, 'from', ctx, errors)
  const fromPort = requireString(raw, 'fromPort', ctx, errors)
  const to = requireString(raw, 'to', ctx, errors)
  const toPort = requireString(raw, 'toPort', ctx, errors)
  const categoryStr = requireString(raw, 'category', ctx, errors)

  if (errors.length > 0) return errors

  if (categoryStr !== 'data' && categoryStr !== 'resource') {
    return [{ message: `${ctx}: unknown edge category: ${categoryStr}` }]
  }

  const from_ = from!
  const fromPort_ = fromPort!
  const to_ = to!
  const toPort_ = toPort!

  const id =
    typeof raw['id'] === 'string'
      ? raw['id']
      : `${from_}:${fromPort_}->${to_}:${toPort_}`

  return {
    id,
    sourceNode: from_,
    sourcePort: fromPort_,
    targetNode: to_,
    targetPort: toPort_,
    category: categoryStr,
  }
}

// ---------------------------------------------------------------------------
// Low-level helpers
// ---------------------------------------------------------------------------

function isObject(val: unknown): val is Record<string, unknown> {
  return typeof val === 'object' && val !== null && !Array.isArray(val)
}

function requireString(
  obj: Record<string, unknown>,
  key: string,
  ctx: string,
  errors: SpecParseError[],
): string | undefined {
  const val = obj[key]
  if (typeof val !== 'string') {
    errors.push({ message: `${ctx}: missing required field: ${key}` })
    return undefined
  }
  return val
}

function requireNumber(
  obj: Record<string, unknown>,
  key: string,
  ctx: string,
  errors: SpecParseError[],
): number | undefined {
  const val = obj[key]
  if (typeof val !== 'number') {
    errors.push({ message: `${ctx}: missing required field: ${key}` })
    return undefined
  }
  return val
}

function requireInteger(
  obj: Record<string, unknown>,
  key: string,
  ctx: string,
  errors: SpecParseError[],
): number | undefined {
  const val = obj[key]
  if (typeof val !== 'number' || !Number.isInteger(val)) {
    errors.push({ message: `${ctx}: ${key}: expected an integer` })
    return undefined
  }
  return val
}

function optString(
  obj: Record<string, unknown>,
  key: string,
): string | undefined {
  const val = obj[key]
  return typeof val === 'string' ? val : undefined
}

function optNumber(
  obj: Record<string, unknown>,
  key: string,
): number | undefined {
  const val = obj[key]
  return typeof val === 'number' ? val : undefined
}

function optInteger(
  obj: Record<string, unknown>,
  key: string,
): number | undefined {
  const val = obj[key]
  return typeof val === 'number' && Number.isInteger(val) ? val : undefined
}
