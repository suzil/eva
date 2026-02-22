{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Eva.Core.Types
  ( -- * ID newtypes
    ProgramId (..)
  , NodeId (..)
  , EdgeId (..)
  , RunId (..)
  , StepId (..)
  , CredentialId (..)

    -- * State machines
  , ProgramState (..)
  , RunState (..)
  , StepState (..)

    -- * Port system
  , PortCategory (..)
  , PortName (..)
  , PortSpec (..)

    -- * Credentials
  , CredentialType (..)
  , Credential (..)

    -- * Config sub-types
  , ResponseFormat (..)
  , ContentSource (..)
  , KnowledgeFormat (..)
  , RefreshPolicy (..)
  , SystemType (..)
  , ActionOperation (..)
  , ErrorHandlingMode (..)
  , TriggerType (..)
  , BackoffStrategy (..)

    -- * Node config types
  , AgentConfig (..)
  , KnowledgeConfig (..)
  , ConnectorConfig (..)
  , ActionConfig (..)
  , TriggerConfig (..)

    -- * Graph primitives
  , NodeType (..)
  , Node (..)
  , Edge (..)
  , Graph (..)

    -- * Messaging
  , MessageMeta (..)
  , Message (..)

    -- * Execution
  , ResourceBindings (..)
  , RetryPolicy (..)
  , Run (..)
  , Step (..)

    -- * Program
  , Program (..)

    -- * Validation
  , ValidationError (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isUpper, toLower)
import Data.Map.Strict (Map)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Build Aeson Options that strip a known camelCase prefix from field labels
-- and lowercase the first character of the remainder.
-- e.g. dropPrefix "agent" "agentSystemPrompt" = "systemPrompt"
dropPrefix :: String -> Options
dropPrefix prefix =
  defaultOptions
    { fieldLabelModifier = lowerFirst . drop (length prefix)
    , omitNothingFields = True
    }
  where
    lowerFirst [] = []
    lowerFirst (c : cs) = toLower c : cs

-- | Strip an uppercase suffix from a constructor tag, then lowercase the whole
-- thing. Used for NodeType: "AgentNode" -> "agent".
dropSuffix :: String -> String -> String
dropSuffix suffix s
  | suffix `isSuffixOf` s = map toLower (take (length s - length suffix) s)
  | otherwise = map toLower s
  where
    isSuffixOf suf str = reverse suf == take (length suf) (reverse str)

-- ---------------------------------------------------------------------------
-- ID Newtypes
-- ---------------------------------------------------------------------------

newtype ProgramId = ProgramId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype NodeId = NodeId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype EdgeId = EdgeId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype RunId = RunId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype StepId = StepId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

newtype CredentialId = CredentialId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

-- ---------------------------------------------------------------------------
-- State Machines
-- ---------------------------------------------------------------------------

data ProgramState = Draft | Active | Paused | Archived
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

programStateOptions :: Options
programStateOptions = defaultOptions{constructorTagModifier = map toLower}

instance ToJSON ProgramState where
  toJSON = genericToJSON programStateOptions
  toEncoding = genericToEncoding programStateOptions

instance FromJSON ProgramState where
  parseJSON = genericParseJSON programStateOptions

-- Constructors are prefixed with "Run" to avoid clashing with StepState.
-- JSON strips the prefix: RunPending -> "pending"
data RunState
  = RunPending
  | RunRunning
  | RunWaiting
  | RunCompleted
  | RunFailed
  | RunCanceled
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

runStateOptions :: Options
runStateOptions = defaultOptions{constructorTagModifier = map toLower . drop 3}

instance ToJSON RunState where
  toJSON = genericToJSON runStateOptions
  toEncoding = genericToEncoding runStateOptions

instance FromJSON RunState where
  parseJSON = genericParseJSON runStateOptions

data StepState
  = StepPending
  | StepRunning
  | StepCompleted
  | StepFailed
  | StepSkipped
  | StepWaiting
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

stepStateOptions :: Options
stepStateOptions = defaultOptions{constructorTagModifier = map toLower . drop 4}

instance ToJSON StepState where
  toJSON = genericToJSON stepStateOptions
  toEncoding = genericToEncoding stepStateOptions

instance FromJSON StepState where
  parseJSON = genericParseJSON stepStateOptions

-- ---------------------------------------------------------------------------
-- Port System
-- ---------------------------------------------------------------------------

data PortCategory = PortData | PortResource
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

portCategoryOptions :: Options
portCategoryOptions = defaultOptions{constructorTagModifier = map toLower . drop 4}

instance ToJSON PortCategory where
  toJSON = genericToJSON portCategoryOptions
  toEncoding = genericToEncoding portCategoryOptions

instance FromJSON PortCategory where
  parseJSON = genericParseJSON portCategoryOptions

newtype PortName = PortName Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

-- | Descriptor for a port on a node type. Used by M2 validation.
data PortSpec = PortSpec
  { portSpecName :: PortName
  , portSpecCategory :: PortCategory
  , portSpecOptional :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON PortSpec where
  toJSON = genericToJSON (dropPrefix "portSpec")
  toEncoding = genericToEncoding (dropPrefix "portSpec")

instance FromJSON PortSpec where
  parseJSON = genericParseJSON (dropPrefix "portSpec")

-- ---------------------------------------------------------------------------
-- Config Sub-types
-- ---------------------------------------------------------------------------

data ResponseFormat = ResponseText | ResponseJson
  deriving stock (Eq, Show, Generic, Enum, Bounded)

instance ToJSON ResponseFormat where
  toJSON ResponseText = "text"
  toJSON ResponseJson = "json"

instance FromJSON ResponseFormat where
  parseJSON = withText "ResponseFormat" $ \case
    "text" -> pure ResponseText
    "json" -> pure ResponseJson
    other -> fail $ "Unknown ResponseFormat: " <> show other

-- | Where a Knowledge node's content comes from.
data ContentSource
  = InlineText Text
  | FileRef Text
  | UrlRef Text
  | UpstreamPort
  deriving stock (Eq, Show, Generic)

contentSourceOptions :: Options
contentSourceOptions =
  defaultOptions
    { sumEncoding = TaggedObject{tagFieldName = "type", contentsFieldName = "value"}
    , constructorTagModifier = camelToSnake
    }
  where
    camelToSnake [] = []
    camelToSnake (c : cs)
      | isUpper c = '_' : toLower c : camelToSnake cs
      | otherwise = c : camelToSnake cs

instance ToJSON ContentSource where
  toJSON = genericToJSON contentSourceOptions
  toEncoding = genericToEncoding contentSourceOptions

instance FromJSON ContentSource where
  parseJSON = genericParseJSON contentSourceOptions

data KnowledgeFormat = FormatText | FormatJson | FormatEmbedded
  deriving stock (Eq, Show, Generic, Enum, Bounded)

instance ToJSON KnowledgeFormat where
  toJSON FormatText = "text"
  toJSON FormatJson = "json"
  toJSON FormatEmbedded = "embedded"

instance FromJSON KnowledgeFormat where
  parseJSON = withText "KnowledgeFormat" $ \case
    "text" -> pure FormatText
    "json" -> pure FormatJson
    "embedded" -> pure FormatEmbedded
    other -> fail $ "Unknown KnowledgeFormat: " <> show other

data RefreshPolicy
  = RefreshStatic
  | RefreshOnRun
  | RefreshPeriodic Int
  deriving stock (Eq, Show, Generic)

instance ToJSON RefreshPolicy where
  toJSON RefreshStatic = object ["type" .= ("static" :: Text)]
  toJSON RefreshOnRun = object ["type" .= ("on_run" :: Text)]
  toJSON (RefreshPeriodic secs) = object ["type" .= ("periodic" :: Text), "periodSeconds" .= secs]

instance FromJSON RefreshPolicy where
  parseJSON = withObject "RefreshPolicy" $ \o -> do
    t <- o .: "type" :: Parser Text
    case t of
      "static" -> pure RefreshStatic
      "on_run" -> pure RefreshOnRun
      "periodic" -> RefreshPeriodic <$> o .: "periodSeconds"
      other -> fail $ "Unknown RefreshPolicy type: " <> show other

data SystemType
  = SystemLinear
  | SystemGitHub
  | SystemHttp
  | SystemCodebase
  deriving stock (Eq, Show, Generic, Enum, Bounded)

systemTypeOptions :: Options
systemTypeOptions = defaultOptions{constructorTagModifier = map toLower . drop 6}

instance ToJSON SystemType where
  toJSON = genericToJSON systemTypeOptions
  toEncoding = genericToEncoding systemTypeOptions

instance FromJSON SystemType where
  parseJSON = genericParseJSON systemTypeOptions

data ActionOperation = OpTemplate | OpCode | OpApiCall | OpFormat
  deriving stock (Eq, Show, Generic, Enum, Bounded)

instance ToJSON ActionOperation where
  toJSON OpTemplate = "template"
  toJSON OpCode = "code"
  toJSON OpApiCall = "api_call"
  toJSON OpFormat = "format"

instance FromJSON ActionOperation where
  parseJSON = withText "ActionOperation" $ \case
    "template" -> pure OpTemplate
    "code" -> pure OpCode
    "api_call" -> pure OpApiCall
    "format" -> pure OpFormat
    other -> fail $ "Unknown ActionOperation: " <> show other

data ErrorHandlingMode
  = ErrFail
  | ErrContinue
  | ErrUseDefault Text
  deriving stock (Eq, Show, Generic)

instance ToJSON ErrorHandlingMode where
  toJSON ErrFail = object ["mode" .= ("fail" :: Text)]
  toJSON ErrContinue = object ["mode" .= ("continue" :: Text)]
  toJSON (ErrUseDefault v) = object ["mode" .= ("use_default" :: Text), "value" .= v]

instance FromJSON ErrorHandlingMode where
  parseJSON = withObject "ErrorHandlingMode" $ \o -> do
    m <- o .: "mode" :: Parser Text
    case m of
      "fail" -> pure ErrFail
      "continue" -> pure ErrContinue
      "use_default" -> ErrUseDefault <$> o .: "value"
      other -> fail $ "Unknown ErrorHandlingMode: " <> show other

data TriggerType
  = TriggerManual
  | TriggerCron
  | TriggerWebhook
  | TriggerConnectorEvent
  deriving stock (Eq, Show, Generic, Enum, Bounded)

triggerTypeOptions :: Options
triggerTypeOptions = defaultOptions{constructorTagModifier = map toLower . drop 7}

instance ToJSON TriggerType where
  toJSON = genericToJSON triggerTypeOptions
  toEncoding = genericToEncoding triggerTypeOptions

instance FromJSON TriggerType where
  parseJSON = genericParseJSON triggerTypeOptions

data BackoffStrategy
  = BackoffFixed Int
  | BackoffExponential Int Int
  deriving stock (Eq, Show, Generic)

backoffOptions :: Options
backoffOptions =
  defaultOptions
    { sumEncoding = TaggedObject{tagFieldName = "strategy", contentsFieldName = "params"}
    , constructorTagModifier = map toLower . drop 7
    }

instance ToJSON BackoffStrategy where
  toJSON = genericToJSON backoffOptions
  toEncoding = genericToEncoding backoffOptions

instance FromJSON BackoffStrategy where
  parseJSON = genericParseJSON backoffOptions

-- ---------------------------------------------------------------------------
-- Node Config Types
-- ---------------------------------------------------------------------------

data AgentConfig = AgentConfig
  { agentModel :: Text
  , agentSystemPrompt :: Text
  , agentResponseFormat :: ResponseFormat
  , agentTemperature :: Double
  , agentMaxTokens :: Maybe Int
  , agentMaxIterations :: Int
  , agentCostBudgetUsd :: Maybe Double
  , agentRetryPolicy :: Maybe RetryPolicy
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AgentConfig where
  toJSON = genericToJSON (dropPrefix "agent")
  toEncoding = genericToEncoding (dropPrefix "agent")

instance FromJSON AgentConfig where
  parseJSON = genericParseJSON (dropPrefix "agent")

data KnowledgeConfig = KnowledgeConfig
  { knowledgeSource :: ContentSource
  , knowledgeFormat :: KnowledgeFormat
  , knowledgeRefreshPolicy :: RefreshPolicy
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON KnowledgeConfig where
  toJSON = genericToJSON (dropPrefix "knowledge")
  toEncoding = genericToEncoding (dropPrefix "knowledge")

instance FromJSON KnowledgeConfig where
  parseJSON = genericParseJSON (dropPrefix "knowledge")

data ConnectorConfig = ConnectorConfig
  { connectorSystem :: SystemType
  , connectorCredentialId :: Maybe Text
  , connectorEndpoint :: Maybe Text
  , connectorScope :: Maybe Text
  , connectorActionFilter :: [Text]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ConnectorConfig where
  toJSON = genericToJSON (dropPrefix "connector")
  toEncoding = genericToEncoding (dropPrefix "connector")

instance FromJSON ConnectorConfig where
  parseJSON = genericParseJSON (dropPrefix "connector")

data ActionConfig = ActionConfig
  { actionOperation :: ActionOperation
  , actionParameters :: Value
  , actionErrorHandling :: ErrorHandlingMode
  , actionRetryPolicy :: Maybe RetryPolicy
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ActionConfig where
  toJSON = genericToJSON (dropPrefix "action")
  toEncoding = genericToEncoding (dropPrefix "action")

instance FromJSON ActionConfig where
  parseJSON = genericParseJSON (dropPrefix "action")

data TriggerConfig = TriggerConfig
  { triggerType :: TriggerType
  , triggerSchedule :: Maybe Text
  , triggerEventFilter :: Maybe Text
  , triggerPayloadTemplate :: Maybe Value
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON TriggerConfig where
  toJSON = genericToJSON (dropPrefix "trigger")
  toEncoding = genericToEncoding (dropPrefix "trigger")

instance FromJSON TriggerConfig where
  parseJSON = genericParseJSON (dropPrefix "trigger")

-- ---------------------------------------------------------------------------
-- Graph Primitives
-- ---------------------------------------------------------------------------

-- | 5 MLP node types. Gate, Collector, Checkpoint deferred to post-MLP.
data NodeType
  = AgentNode AgentConfig
  | KnowledgeNode KnowledgeConfig
  | ConnectorNode ConnectorConfig
  | ActionNode ActionConfig
  | TriggerNode TriggerConfig
  deriving stock (Eq, Show, Generic)

nodeTypeOptions :: Options
nodeTypeOptions =
  defaultOptions
    { sumEncoding = TaggedObject{tagFieldName = "type", contentsFieldName = "config"}
    , constructorTagModifier = dropSuffix "Node"
    }

instance ToJSON NodeType where
  toJSON = genericToJSON nodeTypeOptions
  toEncoding = genericToEncoding nodeTypeOptions

instance FromJSON NodeType where
  parseJSON = genericParseJSON nodeTypeOptions

data Node = Node
  { nodeId :: NodeId
  , nodeLabel :: Text
  , nodeType :: NodeType
  , nodePosX :: Double
  , nodePosY :: Double
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Node where
  toJSON = genericToJSON (dropPrefix "node")
  toEncoding = genericToEncoding (dropPrefix "node")

instance FromJSON Node where
  parseJSON = genericParseJSON (dropPrefix "node")

-- | Directed edge connecting an output port to a compatible input port.
-- Both sourcePort and targetPort are PortName values; category constrains
-- which ports are valid endpoints (data -> data, resource -> resource).
data Edge = Edge
  { edgeId :: EdgeId
  , edgeSourceNode :: NodeId
  , edgeSourcePort :: PortName
  , edgeTargetNode :: NodeId
  , edgeTargetPort :: PortName
  , edgeCategory :: PortCategory
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Edge where
  toJSON = genericToJSON (dropPrefix "edge")
  toEncoding = genericToEncoding (dropPrefix "edge")

instance FromJSON Edge where
  parseJSON = genericParseJSON (dropPrefix "edge")

data Graph = Graph
  { graphNodes :: Map NodeId Node
  , graphEdges :: [Edge]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Graph where
  toJSON = genericToJSON (dropPrefix "graph")
  toEncoding = genericToEncoding (dropPrefix "graph")

instance FromJSON Graph where
  parseJSON = genericParseJSON (dropPrefix "graph")

-- ---------------------------------------------------------------------------
-- Messaging
-- ---------------------------------------------------------------------------

data MessageMeta = MessageMeta
  { metaTraceId :: Text
  , metaTimestamp :: UTCTime
  , metaSourceNode :: NodeId
  , metaRunId :: RunId
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON MessageMeta where
  toJSON = genericToJSON (dropPrefix "meta")
  toEncoding = genericToEncoding (dropPrefix "meta")

instance FromJSON MessageMeta where
  parseJSON = genericParseJSON (dropPrefix "meta")

data Message = Message
  { msgType :: Text
  , msgPayload :: Value
  , msgMeta :: MessageMeta
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Message where
  toJSON = genericToJSON (dropPrefix "msg")
  toEncoding = genericToEncoding (dropPrefix "msg")

instance FromJSON Message where
  parseJSON = genericParseJSON (dropPrefix "msg")

-- ---------------------------------------------------------------------------
-- Execution
-- ---------------------------------------------------------------------------

-- | Resolved resource connections for a node at dispatch time.
-- Knowledge content sources and Connector capability configs, collected
-- from resource edges pointing at the node. Static grants — not runtime messages.
data ResourceBindings = ResourceBindings
  { rbKnowledge  :: [KnowledgeConfig]
  , rbConnectors :: [ConnectorConfig]
  }

data RetryPolicy = RetryPolicy
  { retryMaxAttempts :: Int
  , retryBackoff :: BackoffStrategy
  , retryTimeoutMs :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RetryPolicy where
  toJSON = genericToJSON (dropPrefix "retry")
  toEncoding = genericToEncoding (dropPrefix "retry")

instance FromJSON RetryPolicy where
  parseJSON = genericParseJSON (dropPrefix "retry")

data Run = Run
  { runId :: RunId
  , runProgramId :: ProgramId
  , runState :: RunState
  , runTriggerInfo :: Maybe Value
  , runStartedAt :: Maybe UTCTime
  , runFinishedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Run where
  toJSON = genericToJSON (dropPrefix "run")
  toEncoding = genericToEncoding (dropPrefix "run")

instance FromJSON Run where
  parseJSON = genericParseJSON (dropPrefix "run")

data Step = Step
  { stepId :: StepId
  , stepRunId :: RunId
  , stepNodeId :: NodeId
  , stepState :: StepState
  , stepInput :: Maybe Value
  , stepOutput :: Maybe Value
  , stepError :: Maybe Text
  , stepRetryCount :: Int
  , stepStartedAt :: Maybe UTCTime
  , stepFinishedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Step where
  toJSON = genericToJSON (dropPrefix "step")
  toEncoding = genericToEncoding (dropPrefix "step")

instance FromJSON Step where
  parseJSON = genericParseJSON (dropPrefix "step")

-- ---------------------------------------------------------------------------
-- Credentials
-- ---------------------------------------------------------------------------

data CredentialType = CredentialApiKey | CredentialOAuthToken
  deriving stock (Eq, Show, Generic, Enum, Bounded)

instance ToJSON CredentialType where
  toJSON CredentialApiKey    = "api_key"
  toJSON CredentialOAuthToken = "oauth_token"

instance FromJSON CredentialType where
  parseJSON = withText "CredentialType" $ \case
    "api_key"     -> pure CredentialApiKey
    "oauth_token" -> pure CredentialOAuthToken
    other         -> fail $ "Unknown CredentialType: " <> show other

-- | Safe credential view — never includes the encrypted secret.
data Credential = Credential
  { credentialId        :: CredentialId
  , credentialName      :: Text
  , credentialSystem    :: SystemType
  , credentialType      :: CredentialType
  , credentialCreatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Credential where
  toJSON = genericToJSON (dropPrefix "credential")
  toEncoding = genericToEncoding (dropPrefix "credential")

instance FromJSON Credential where
  parseJSON = genericParseJSON (dropPrefix "credential")

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

data ValidationError = ValidationError
  { veMessage :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ValidationError where
  toJSON e = object ["message" .= veMessage e]

instance FromJSON ValidationError where
  parseJSON = withObject "ValidationError" $ \o ->
    ValidationError <$> o .: "message"

-- ---------------------------------------------------------------------------
-- Program
-- ---------------------------------------------------------------------------

data Program = Program
  { programId :: ProgramId
  , programName :: Text
  , programState :: ProgramState
  , programGraph :: Graph
  , programCreatedAt :: UTCTime
  , programUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Program where
  toJSON = genericToJSON (dropPrefix "program")
  toEncoding = genericToEncoding (dropPrefix "program")

instance FromJSON Program where
  parseJSON = genericParseJSON (dropPrefix "program")
