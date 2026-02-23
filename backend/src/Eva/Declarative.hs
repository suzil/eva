{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Bidirectional YAML serialisation for 'Graph'.
--
-- Produces a human-readable "promoted-fields" schema: node config keys are
-- inlined at the same level as @type@, @label@, @posX@, @posY@.
-- 'TriggerConfig' uses @triggerType@ instead of @type@ to avoid colliding
-- with the node-level @type@ field. The JSON REST API is unaffected.
--
-- Minimum schema:
-- @
-- eva.version: "1"
-- nodes:
--   \<nodeId\>:
--     type: agent | knowledge | connector | action | trigger
--     label: \<string\>
--     posX: \<float\>
--     posY: \<float\>
--     # ... promoted config fields ...
-- edges:
--   - from: \<nodeId\>
--     fromPort: \<portName\>
--     to: \<nodeId\>
--     toPort: \<portName\>
--     category: data | resource
-- @
module Eva.Declarative
  ( graphToYaml
  , yamlToGraph
  , ParseError (..)
  ) where

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (fromFloatDigits, floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.YAML ((.=))
import qualified Data.YAML as Y

import Eva.Core.Types

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | A structured error from 'yamlToGraph'.
data ParseError = ParseError { peMessage :: Text }
  deriving (Eq, Show)

-- | Serialise a 'Graph' to a YAML document (promoted-fields format).
graphToYaml :: Graph -> Text
graphToYaml g =
  TE.decodeUtf8 $ Y.encode1Strict (buildDoc g)

-- | Parse YAML produced by 'graphToYaml' back to a 'Graph'.
-- Returns @Left@ with a non-empty list of errors on any failure.
yamlToGraph :: Text -> Either [ParseError] Graph
yamlToGraph t =
  case Y.decode1Strict (TE.encodeUtf8 t) :: Either (Y.Pos, String) YamlGraphDoc of
    Left (_, msg) -> Left [ParseError (T.pack msg)]
    Right (YamlGraphDoc result) -> result

-- ---------------------------------------------------------------------------
-- FromYAML wrapper — lets HsYAML handle YAML syntax; we handle semantics
-- ---------------------------------------------------------------------------

newtype YamlGraphDoc = YamlGraphDoc (Either [ParseError] Graph)

instance Y.FromYAML YamlGraphDoc where
  parseYAML node = pure (YamlGraphDoc (parseDoc node))

-- ---------------------------------------------------------------------------
-- Encoding: Graph -> Node ()
-- ---------------------------------------------------------------------------

buildDoc :: Graph -> Y.Node ()
buildDoc g =
  Y.mapping
    [ "eva.version" .= ("1" :: Text)
    , "nodes" .= Y.mapping (map buildNodeEntry (Map.toList (graphNodes g)))
    , "edges" .= map buildEdge (graphEdges g)
    ]

buildNodeEntry :: (NodeId, Node) -> (Y.Node (), Y.Node ())
buildNodeEntry (NodeId nid, n) = nid .= buildEvaNode n

buildEvaNode :: Node -> Y.Node ()
buildEvaNode n =
  Y.mapping $
    [ "type"  .= nodeTypeName (nodeType n)
    , "label" .= nodeLabel n
    , "posX"  .= nodePosX n
    , "posY"  .= nodePosY n
    ]
    ++ buildNodeConfig (nodeType n)

nodeTypeName :: NodeType -> Text
nodeTypeName (AgentNode _)     = "agent"
nodeTypeName (KnowledgeNode _) = "knowledge"
nodeTypeName (ConnectorNode _) = "connector"
nodeTypeName (ActionNode _)    = "action"
nodeTypeName (TriggerNode _)   = "trigger"

buildNodeConfig :: NodeType -> [(Y.Node (), Y.Node ())]
buildNodeConfig (AgentNode cfg) =
  [ "model"          .= agentModel cfg
  , "systemPrompt"   .= agentSystemPrompt cfg
  , "responseFormat" .= encodeRF (agentResponseFormat cfg)
  , "temperature"    .= agentTemperature cfg
  , "maxIterations"  .= agentMaxIterations cfg
  ]
  ++ maybe [] (\p  -> ["provider"      .= encodeProv p])      (agentProvider cfg)
  ++ maybe [] (\t  -> ["maxTokens"     .= t])                 (agentMaxTokens cfg)
  ++ maybe [] (\b  -> ["costBudgetUsd" .= b])                 (agentCostBudgetUsd cfg)
  ++ maybe [] (\rp -> ["retryPolicy"   .= buildRetryPolicy rp]) (agentRetryPolicy cfg)
  where
    encodeRF ResponseText = "text" :: Text
    encodeRF ResponseJson = "json"
    encodeProv ProviderOpenAI    = "openai" :: Text
    encodeProv ProviderAnthropic = "anthropic"

buildNodeConfig (KnowledgeNode cfg) =
  [ "source"        .= buildContentSource (knowledgeSource cfg)
  , "format"        .= encodeKF (knowledgeFormat cfg)
  , "refreshPolicy" .= buildRefreshPolicy (knowledgeRefreshPolicy cfg)
  ]
  where
    encodeKF FormatText     = "text" :: Text
    encodeKF FormatJson     = "json"
    encodeKF FormatEmbedded = "embedded"

buildNodeConfig (ConnectorNode cfg) =
  [ "system"       .= encodeSystem (connectorSystem cfg)
  , "actionFilter" .= connectorActionFilter cfg
  ]
  ++ maybe [] (\c -> ["credentialId" .= c]) (connectorCredentialId cfg)
  ++ maybe [] (\e -> ["endpoint"     .= e]) (connectorEndpoint cfg)
  ++ maybe [] (\s -> ["scope"        .= s]) (connectorScope cfg)
  where
    encodeSystem SystemLinear   = "linear" :: Text
    encodeSystem SystemGitHub   = "github"
    encodeSystem SystemHttp     = "http"
    encodeSystem SystemCodebase = "codebase"

buildNodeConfig (ActionNode cfg) =
  [ "operation"     .= encodeOp (actionOperation cfg)
  , "parameters"    .= buildAesonValue (actionParameters cfg)
  , "errorHandling" .= buildErrorHandling (actionErrorHandling cfg)
  ]
  ++ maybe [] (\rp -> ["retryPolicy" .= buildRetryPolicy rp]) (actionRetryPolicy cfg)
  where
    encodeOp OpTemplate = "template" :: Text
    encodeOp OpCode     = "code"
    encodeOp OpApiCall  = "api_call"
    encodeOp OpFormat   = "format"

buildNodeConfig (TriggerNode cfg) =
  -- "triggerType" avoids collision with the node-level "type" field
  [ "triggerType" .= encodeTT (triggerType cfg)
  ]
  ++ maybe [] (\s -> ["schedule"        .= s])                    (triggerSchedule cfg)
  ++ maybe [] (\f -> ["eventFilter"     .= f])                    (triggerEventFilter cfg)
  ++ maybe [] (\p -> ["payloadTemplate" .= buildAesonValue p])    (triggerPayloadTemplate cfg)
  where
    encodeTT TriggerManual         = "manual" :: Text
    encodeTT TriggerCron           = "cron"
    encodeTT TriggerWebhook        = "webhook"
    encodeTT TriggerConnectorEvent = "connectorevent"

buildEdge :: Edge -> Y.Node ()
buildEdge e =
  Y.mapping
    [ "id"       .= unEdgeId   (edgeId e)
    , "from"     .= unNodeId   (edgeSourceNode e)
    , "fromPort" .= unPortName (edgeSourcePort e)
    , "to"       .= unNodeId   (edgeTargetNode e)
    , "toPort"   .= unPortName (edgeTargetPort e)
    , "category" .= encodePortCat (edgeCategory e)
    ]
  where
    encodePortCat PortData     = "data" :: Text
    encodePortCat PortResource = "resource"

buildContentSource :: ContentSource -> Y.Node ()
buildContentSource (InlineText t) =
  Y.mapping ["type" .= ("_inline_text" :: Text), "value" .= t]
buildContentSource (FileRef t) =
  Y.mapping ["type" .= ("_file_ref" :: Text), "value" .= t]
buildContentSource (UrlRef t) =
  Y.mapping ["type" .= ("_url_ref" :: Text), "value" .= t]
buildContentSource UpstreamPort =
  Y.mapping ["type" .= ("_upstream_port" :: Text)]

buildRefreshPolicy :: RefreshPolicy -> Y.Node ()
buildRefreshPolicy RefreshStatic =
  Y.mapping ["type" .= ("static" :: Text)]
buildRefreshPolicy RefreshOnRun =
  Y.mapping ["type" .= ("on_run" :: Text)]
buildRefreshPolicy (RefreshPeriodic secs) =
  Y.mapping ["type" .= ("periodic" :: Text), "periodSeconds" .= secs]

buildErrorHandling :: ErrorHandlingMode -> Y.Node ()
buildErrorHandling ErrFail =
  Y.mapping ["mode" .= ("fail" :: Text)]
buildErrorHandling ErrContinue =
  Y.mapping ["mode" .= ("continue" :: Text)]
buildErrorHandling (ErrUseDefault val) =
  Y.mapping ["mode" .= ("use_default" :: Text), "value" .= val]

buildRetryPolicy :: RetryPolicy -> Y.Node ()
buildRetryPolicy rp =
  Y.mapping $
    [ "maxAttempts" .= retryMaxAttempts rp
    , "backoff"     .= buildBackoff (retryBackoff rp)
    ]
    ++ maybe [] (\t -> ["timeoutMs" .= t]) (retryTimeoutMs rp)
  where
    buildBackoff (BackoffFixed n) =
      Y.mapping ["strategy" .= ("fixed" :: Text), "params" .= n]
    buildBackoff (BackoffExponential base cap) =
      Y.mapping
        [ "strategy" .= ("exponential" :: Text)
        , "params"   .= [base, cap]
        ]

-- | Convert an Aeson 'Value' to a HsYAML 'Node' for embedding in YAML output.
buildAesonValue :: Ae.Value -> Y.Node ()
buildAesonValue v = case v of
  Ae.Null     -> Y.Scalar () Y.SNull
  Ae.Bool b   -> Y.toYAML b
  Ae.Number n ->
    case floatingOrInteger n of
      Left  d -> Y.toYAML (d :: Double)
      Right i -> Y.toYAML (i :: Integer)
  Ae.String t -> Y.toYAML t
  Ae.Array arr ->
    Y.toYAML (map buildAesonValue (V.toList arr))
  Ae.Object obj ->
    Y.mapping
      [ AKey.toText k .= buildAesonValue val
      | (k, val) <- AKM.toList obj
      ]

-- ---------------------------------------------------------------------------
-- Decoding: Node Pos -> Graph
-- ---------------------------------------------------------------------------

-- Key-value pairs extracted from a YAML mapping (Map converted to list).
type Pairs = [(Y.Node Y.Pos, Y.Node Y.Pos)]

parseDoc :: Y.Node Y.Pos -> Either [ParseError] Graph
parseDoc node = do
  pairs      <- asMapping "top-level document" node
  _ver       <- lookupText "eva.version" pairs
  nodesPairs <- lookupMappingPairs "nodes" pairs
  edgesSeq   <- lookupSequence "edges" pairs
  nodes      <- parseNodes nodesPairs
  edges      <- parseEdges edgesSeq
  pure $ Graph nodes edges

parseNodes :: Pairs -> Either [ParseError] (Map NodeId Node)
parseNodes pairs = do
  entries <- traverse parseNodeEntry pairs
  pure $ Map.fromList entries

parseNodeEntry :: (Y.Node Y.Pos, Y.Node Y.Pos) -> Either [ParseError] (NodeId, Node)
parseNodeEntry (keyNode, valNode) = do
  nid <- NodeId <$> asText "node key" keyNode
  n   <- parseEvaNode nid valNode
  pure (nid, n)

parseEvaNode :: NodeId -> Y.Node Y.Pos -> Either [ParseError] Node
parseEvaNode nid node = do
  pairs   <- asMapping "node" node
  typeStr <- lookupText "type" pairs
  label   <- lookupText "label" pairs
  posX    <- lookupDouble "posX" pairs
  posY    <- lookupDouble "posY" pairs
  nt      <- parseNodeType typeStr pairs
  pure $ Node
    { nodeId    = nid
    , nodeLabel = label
    , nodeType  = nt
    , nodePosX  = posX
    , nodePosY  = posY
    }

parseNodeType :: Text -> Pairs -> Either [ParseError] NodeType
parseNodeType "agent"     pairs = AgentNode     <$> parseAgentConfig pairs
parseNodeType "knowledge" pairs = KnowledgeNode <$> parseKnowledgeConfig pairs
parseNodeType "connector" pairs = ConnectorNode <$> parseConnectorConfig pairs
parseNodeType "action"    pairs = ActionNode    <$> parseActionConfig pairs
parseNodeType "trigger"   pairs = TriggerNode   <$> parseTriggerConfig pairs
parseNodeType other       _     = Left [ParseError $ "unknown node type: " <> other]

parseAgentConfig :: Pairs -> Either [ParseError] AgentConfig
parseAgentConfig pairs = do
  model          <- lookupText   "model"        pairs
  systemPrompt   <- lookupText   "systemPrompt" pairs
  responseFormat <- lookupText   "responseFormat" pairs >>= \case
    "text" -> pure ResponseText
    "json" -> pure ResponseJson
    other  -> Left [ParseError $ "unknown responseFormat: " <> other]
  temperature   <- lookupDouble "temperature"   pairs
  maxIterations <- lookupInt    "maxIterations" pairs
  provider      <- optField $ lookupText "provider" pairs >>= \case
    "openai"    -> pure ProviderOpenAI
    "anthropic" -> pure ProviderAnthropic
    other       -> Left [ParseError $ "unknown provider: " <> other]
  maxTokens     <- optField $ lookupInt    "maxTokens"     pairs
  costBudgetUsd <- optField $ lookupDouble "costBudgetUsd" pairs
  retryPolicy   <- optField $ lookupMappingPairs "retryPolicy" pairs >>= parseRetryPolicy
  pure $ AgentConfig
    { agentProvider       = provider
    , agentModel          = model
    , agentSystemPrompt   = systemPrompt
    , agentResponseFormat = responseFormat
    , agentTemperature    = temperature
    , agentMaxTokens      = maxTokens
    , agentMaxIterations  = maxIterations
    , agentCostBudgetUsd  = costBudgetUsd
    , agentRetryPolicy    = retryPolicy
    }

parseKnowledgeConfig :: Pairs -> Either [ParseError] KnowledgeConfig
parseKnowledgeConfig pairs = do
  sourcePairs   <- lookupMappingPairs "source" pairs
  source        <- parseContentSource sourcePairs
  format        <- lookupText "format" pairs >>= \case
    "text"     -> pure FormatText
    "json"     -> pure FormatJson
    "embedded" -> pure FormatEmbedded
    other      -> Left [ParseError $ "unknown knowledge format: " <> other]
  rpPairs       <- lookupMappingPairs "refreshPolicy" pairs
  refreshPolicy <- parseRefreshPolicy rpPairs
  pure $ KnowledgeConfig
    { knowledgeSource        = source
    , knowledgeFormat        = format
    , knowledgeRefreshPolicy = refreshPolicy
    }

parseConnectorConfig :: Pairs -> Either [ParseError] ConnectorConfig
parseConnectorConfig pairs = do
  system <- lookupText "system" pairs >>= \case
    "linear"   -> pure SystemLinear
    "github"   -> pure SystemGitHub
    "http"     -> pure SystemHttp
    "codebase" -> pure SystemCodebase
    other      -> Left [ParseError $ "unknown system type: " <> other]
  actionFilter <- lookupTextList "actionFilter" pairs
  credentialId <- optField $ lookupText "credentialId" pairs
  endpoint     <- optField $ lookupText "endpoint"     pairs
  scope        <- optField $ lookupText "scope"        pairs
  pure $ ConnectorConfig
    { connectorSystem       = system
    , connectorCredentialId = credentialId
    , connectorEndpoint     = endpoint
    , connectorScope        = scope
    , connectorActionFilter = actionFilter
    }

parseActionConfig :: Pairs -> Either [ParseError] ActionConfig
parseActionConfig pairs = do
  operation <- lookupText "operation" pairs >>= \case
    "template" -> pure OpTemplate
    "code"     -> pure OpCode
    "api_call" -> pure OpApiCall
    "format"   -> pure OpFormat
    other      -> Left [ParseError $ "unknown operation: " <> other]
  parameters    <- lookupAesonValue "parameters"    pairs
  ehPairs       <- lookupMappingPairs "errorHandling" pairs
  errorHandling <- parseErrorHandling ehPairs
  retryPolicy   <- optField $ lookupMappingPairs "retryPolicy" pairs >>= parseRetryPolicy
  pure $ ActionConfig
    { actionOperation     = operation
    , actionParameters    = parameters
    , actionErrorHandling = errorHandling
    , actionRetryPolicy   = retryPolicy
    }

parseTriggerConfig :: Pairs -> Either [ParseError] TriggerConfig
parseTriggerConfig pairs = do
  tt <- lookupText "triggerType" pairs >>= \case
    "manual"         -> pure TriggerManual
    "cron"           -> pure TriggerCron
    "webhook"        -> pure TriggerWebhook
    "connectorevent" -> pure TriggerConnectorEvent
    other            -> Left [ParseError $ "unknown triggerType: " <> other]
  schedule        <- optField $ lookupText       "schedule"        pairs
  eventFilter     <- optField $ lookupText       "eventFilter"     pairs
  payloadTemplate <- optField $ lookupAesonValue "payloadTemplate" pairs
  pure $ TriggerConfig
    { triggerType            = tt
    , triggerSchedule        = schedule
    , triggerEventFilter     = eventFilter
    , triggerPayloadTemplate = payloadTemplate
    }

parseContentSource :: Pairs -> Either [ParseError] ContentSource
parseContentSource pairs = do
  t <- lookupText "type" pairs
  case t of
    "_inline_text"   -> InlineText <$> lookupText "value" pairs
    "_file_ref"      -> FileRef    <$> lookupText "value" pairs
    "_url_ref"       -> UrlRef     <$> lookupText "value" pairs
    "_upstream_port" -> pure UpstreamPort
    other            -> Left [ParseError $ "unknown ContentSource type: " <> other]

parseRefreshPolicy :: Pairs -> Either [ParseError] RefreshPolicy
parseRefreshPolicy pairs = do
  t <- lookupText "type" pairs
  case t of
    "static"   -> pure RefreshStatic
    "on_run"   -> pure RefreshOnRun
    "periodic" -> RefreshPeriodic <$> lookupInt "periodSeconds" pairs
    other      -> Left [ParseError $ "unknown refreshPolicy type: " <> other]

parseErrorHandling :: Pairs -> Either [ParseError] ErrorHandlingMode
parseErrorHandling pairs = do
  m <- lookupText "mode" pairs
  case m of
    "fail"        -> pure ErrFail
    "continue"    -> pure ErrContinue
    "use_default" -> ErrUseDefault <$> lookupText "value" pairs
    other         -> Left [ParseError $ "unknown errorHandling mode: " <> other]

parseRetryPolicy :: Pairs -> Either [ParseError] RetryPolicy
parseRetryPolicy pairs = do
  maxAttempts <- lookupInt "maxAttempts" pairs
  bpPairs     <- lookupMappingPairs "backoff" pairs
  backoff     <- parseBackoff bpPairs
  timeoutMs   <- optField $ lookupInt "timeoutMs" pairs
  pure $ RetryPolicy
    { retryMaxAttempts = maxAttempts
    , retryBackoff     = backoff
    , retryTimeoutMs   = timeoutMs
    }

parseBackoff :: Pairs -> Either [ParseError] BackoffStrategy
parseBackoff pairs = do
  s <- lookupText "strategy" pairs
  case s of
    "fixed" -> BackoffFixed <$> lookupInt "params" pairs
    "exponential" -> do
      ns <- lookupIntList "params" pairs
      case ns of
        [b, c] -> pure $ BackoffExponential b c
        _      -> Left [ParseError "exponential backoff params must be a 2-element list"]
    other -> Left [ParseError $ "unknown backoff strategy: " <> other]

parseEdges :: [Y.Node Y.Pos] -> Either [ParseError] [Edge]
parseEdges = traverse parseEdge

parseEdge :: Y.Node Y.Pos -> Either [ParseError] Edge
parseEdge node = do
  pairs    <- asMapping "edge" node
  -- Edge ID: stored in YAML if present; synthesised from endpoints if absent
  meid     <- optField $ EdgeId <$> lookupText "id" pairs
  from     <- NodeId   <$> lookupText "from"     pairs
  fromPort <- PortName <$> lookupText "fromPort" pairs
  to       <- NodeId   <$> lookupText "to"       pairs
  toPort   <- PortName <$> lookupText "toPort"   pairs
  category <- lookupText "category" pairs >>= \case
    "data"     -> pure PortData
    "resource" -> pure PortResource
    other      -> Left [ParseError $ "unknown edge category: " <> other]
  let eid = case meid of
        Just i  -> i
        Nothing -> EdgeId $
          unNodeId from <> ":" <> unPortName fromPort
          <> "->" <> unNodeId to <> ":" <> unPortName toPort
  pure $ Edge
    { edgeId         = eid
    , edgeSourceNode = from
    , edgeSourcePort = fromPort
    , edgeTargetNode = to
    , edgeTargetPort = toPort
    , edgeCategory   = category
    }

-- ---------------------------------------------------------------------------
-- Low-level YAML node navigation
-- ---------------------------------------------------------------------------

-- | In HsYAML 0.2, Y.Mapping stores pairs as Map (Node Pos) (Node Pos).
-- We convert to a list for uniform pattern matching throughout this module.
asMapping :: String -> Y.Node Y.Pos -> Either [ParseError] Pairs
asMapping _ (Y.Mapping _ _ m) = Right (Map.toList m)
asMapping ctx _               = Left [ParseError $ T.pack ctx <> ": expected a mapping"]

-- | In HsYAML 0.2, Y.Scalar takes only 2 args: (pos, Scalar value) — no Tag field.
asText :: String -> Y.Node Y.Pos -> Either [ParseError] Text
asText _ (Y.Scalar _ (Y.SStr t)) = Right t
asText ctx _                      = Left [ParseError $ T.pack ctx <> ": expected a string scalar"]

lookupNode :: Text -> Pairs -> Either [ParseError] (Y.Node Y.Pos)
lookupNode key pairs =
  case [v | (Y.Scalar _ (Y.SStr k), v) <- pairs, k == key] of
    (v:_) -> Right v
    []    -> Left [ParseError $ "missing required field: " <> key]

lookupText :: Text -> Pairs -> Either [ParseError] Text
lookupText key pairs = lookupNode key pairs >>= asText (T.unpack key)

lookupDouble :: Text -> Pairs -> Either [ParseError] Double
lookupDouble key pairs = do
  node <- lookupNode key pairs
  case node of
    Y.Scalar _ (Y.SFloat d) -> Right d
    Y.Scalar _ (Y.SInt   i) -> Right (fromIntegral i)
    _ -> Left [ParseError $ key <> ": expected a number"]

lookupInt :: Text -> Pairs -> Either [ParseError] Int
lookupInt key pairs = do
  node <- lookupNode key pairs
  case node of
    Y.Scalar _ (Y.SInt i) -> Right (fromIntegral i)
    Y.Scalar _ (Y.SFloat d)
      | d == fromIntegral (round d :: Int) -> Right (round d)
    _ -> Left [ParseError $ key <> ": expected an integer"]

lookupMappingPairs :: Text -> Pairs -> Either [ParseError] Pairs
lookupMappingPairs key pairs = lookupNode key pairs >>= asMapping (T.unpack key)

lookupSequence :: Text -> Pairs -> Either [ParseError] [Y.Node Y.Pos]
lookupSequence key pairs = do
  node <- lookupNode key pairs
  case node of
    Y.Sequence _ _ xs -> Right xs
    _                 -> Left [ParseError $ key <> ": expected a sequence"]

lookupTextList :: Text -> Pairs -> Either [ParseError] [Text]
lookupTextList key pairs = lookupSequence key pairs >>= traverse (asText (T.unpack key))

lookupIntList :: Text -> Pairs -> Either [ParseError] [Int]
lookupIntList key pairs = lookupSequence key pairs >>= traverse go
  where
    go (Y.Scalar _ (Y.SInt i)) = Right (fromIntegral i)
    go _                        = Left [ParseError "expected an integer in list"]

-- | Parse a YAML node as an Aeson 'Value' (for @parameters@, @payloadTemplate@, etc.)
lookupAesonValue :: Text -> Pairs -> Either [ParseError] Ae.Value
lookupAesonValue key pairs = lookupNode key pairs >>= nodeToAeson key

nodeToAeson :: Text -> Y.Node Y.Pos -> Either [ParseError] Ae.Value
nodeToAeson ctx node = case node of
  Y.Scalar _ s -> Right $ case s of
    Y.SNull        -> Ae.Null
    Y.SBool b      -> Ae.Bool b
    Y.SFloat d     -> Ae.Number (fromFloatDigits d)
    Y.SInt  i      -> Ae.Number (fromInteger i)
    Y.SStr  t      -> Ae.String t
    Y.SUnknown _ t -> Ae.String t   -- preserve unrecognized scalars as strings
  Y.Mapping _ _ theMap -> do
    entries <- traverse parsePair (Map.toList theMap)
    pure $ Ae.Object (AKM.fromList entries)
    where
      parsePair (kn, vn) = do
        k <- asText (T.unpack ctx) kn
        v <- nodeToAeson k vn
        pure (AKey.fromText k, v)
  Y.Sequence _ _ xs -> do
    vals <- traverse (nodeToAeson ctx) xs
    pure (Ae.Array (V.fromList vals))
  Y.Anchor _ _ inner -> nodeToAeson ctx inner

-- | Make a required lookup optional: missing-field errors become 'Nothing'.
optField :: Either [ParseError] a -> Either [ParseError] (Maybe a)
optField (Right x)  = Right (Just x)
optField (Left errs)
  | all isMissing errs = Right Nothing
  | otherwise          = Left errs
  where
    isMissing (ParseError msg) = "missing required field:" `T.isPrefixOf` msg

-- ---------------------------------------------------------------------------
-- Misc helpers
-- ---------------------------------------------------------------------------

unNodeId :: NodeId -> Text
unNodeId (NodeId t) = t

unPortName :: PortName -> Text
unPortName (PortName t) = t

unEdgeId :: EdgeId -> Text
unEdgeId (EdgeId t) = t
