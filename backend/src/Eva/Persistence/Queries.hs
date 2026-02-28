{-# LANGUAGE OverloadedStrings #-}

-- | Database query functions using persistent / esqueleto.
-- All public functions run inside AppM and use the connection pool from AppEnv.
module Eva.Persistence.Queries
  ( -- * Runner
    runDb

    -- * Programs
  , insertProgram
  , getProgram
  , listPrograms
  , updateProgram
  , deleteProgram
  , putGraph

    -- * Runs
  , insertRun
  , getRun
  , listRunsForProgram
  , updateRun

    -- * Steps
  , insertStep
  , getStep
  , listStepsForRun
  , updateStep
  , updateStepTransition
  , updateStepRetryCount

    -- * Log entries
  , insertLogEntry

    -- * Credentials
  , insertCredential
  , listCredentials
  , deleteCredential
  , getDecryptedCredential

    -- * Codebases
  , insertCodebase
  , getCodebase
  , listCodebasesForProgram
  , deleteCodebase

    -- * Changesets
  , insertChangeset
  , insertFileChange
  , listChangesetsForProgram
  , listChangesetsForRun
  , getChangeset
  , updateFileChangeStatus
  , updateChangesetStatus
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrict, encode, toJSON)
import Data.Aeson.Types (parseEither, parseJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist.Sql
  ( Entity (..)
  , SelectOpt (..)
  , SqlPersistT
  , delete
  , deleteWhere
  , get
  , insertKey
  , runSqlPool
  , selectList
  , update
  , (=.)
  , (==.)
  , (<-.)
  )

import Data.ByteString (ByteString)

import Eva.App (AppEnv (..), AppM)
import Eva.Codebase.Types
import Eva.Core.Types
import qualified Eva.Crypto as Crypto
import Eva.Persistence.Schema

-- ---------------------------------------------------------------------------
-- Runner
-- ---------------------------------------------------------------------------

-- | Lift a DB action into AppM using the app's connection pool.
runDb :: SqlPersistT IO a -> AppM a
runDb action = do
  pool <- asks envDbPool
  liftIO $ runSqlPool action pool

-- ---------------------------------------------------------------------------
-- Encoding helpers
-- ---------------------------------------------------------------------------

-- | Encode a ToJSON value as a compact JSON Text (for JSON-column storage).
toJsonText :: ToJSON a => a -> Text
toJsonText = TE.decodeUtf8 . BL.toStrict . encode

-- | Decode a JSON Text column back to a domain value.
fromJsonText :: FromJSON a => Text -> Either String a
fromJsonText = eitherDecodeStrict . TE.encodeUtf8

-- | Encode a sum-type value as a plain lowercase string (no quotes).
-- Works for any type whose ToJSON instance produces a JSON String.
-- e.g. ProgramState Draft -> "draft", PortCategory PortData -> "data"
encodeState :: ToJSON a => a -> Text
encodeState = T.dropAround (== '"') . toJsonText

-- | Decode a plain state string back to a domain value.
decodeState :: FromJSON a => Text -> Either String a
decodeState t = parseEither parseJSON (toJSON t)

-- ---------------------------------------------------------------------------
-- ID conversion helpers
-- ---------------------------------------------------------------------------

toProgramRowId :: ProgramId -> ProgramRowId
toProgramRowId (ProgramId t) = ProgramRowKey t

fromProgramRowId :: ProgramRowId -> ProgramId
fromProgramRowId (ProgramRowKey t) = ProgramId t

toNodeRowId :: NodeId -> NodeRowId
toNodeRowId (NodeId t) = NodeRowKey t

fromNodeRowId :: NodeRowId -> NodeId
fromNodeRowId (NodeRowKey t) = NodeId t

toEdgeRowId :: EdgeId -> EdgeRowId
toEdgeRowId (EdgeId t) = EdgeRowKey t

fromEdgeRowId :: EdgeRowId -> EdgeId
fromEdgeRowId (EdgeRowKey t) = EdgeId t

toRunRowId :: RunId -> RunRowId
toRunRowId (RunId t) = RunRowKey t

fromRunRowId :: RunRowId -> RunId
fromRunRowId (RunRowKey t) = RunId t

toStepRowId :: StepId -> StepRowId
toStepRowId (StepId t) = StepRowKey t

fromStepRowId :: StepRowId -> StepId
fromStepRowId (StepRowKey t) = StepId t

toCredentialRowId :: CredentialId -> CredentialRowId
toCredentialRowId (CredentialId t) = CredentialRowKey t

fromCredentialRowId :: CredentialRowId -> CredentialId
fromCredentialRowId (CredentialRowKey t) = CredentialId t

-- | Extract the discriminator string for a NodeType (matches JSON "type" field).
nodeTypeTag :: NodeType -> Text
nodeTypeTag (AgentNode{})     = "agent"
nodeTypeTag (KnowledgeNode{}) = "knowledge"
nodeTypeTag (ConnectorNode{}) = "connector"
nodeTypeTag (ActionNode{})    = "action"
nodeTypeTag (TriggerNode{})   = "trigger"

-- ---------------------------------------------------------------------------
-- Row <-> Domain conversions
-- ---------------------------------------------------------------------------

nodeToRow :: ProgramId -> Node -> NodeRow
nodeToRow pid n =
  NodeRow
    { nodeRowProgramId = toProgramRowId pid
    , nodeRowTypeTag   = nodeTypeTag (nodeType n)
    , nodeRowConfig    = toJsonText (nodeType n)
    , nodeRowLabel     = nodeLabel n
    , nodeRowPosX      = nodePosX n
    , nodeRowPosY      = nodePosY n
    }

nodeFromRow :: NodeRowId -> NodeRow -> Either String Node
nodeFromRow rowId row = do
  nt <- fromJsonText (nodeRowConfig row)
  pure Node
    { nodeId    = fromNodeRowId rowId
    , nodeLabel = nodeRowLabel row
    , nodeType  = nt
    , nodePosX  = nodeRowPosX row
    , nodePosY  = nodeRowPosY row
    }

edgeToRow :: ProgramId -> Edge -> EdgeRow
edgeToRow pid e =
  let (NodeId  srcN) = edgeSourceNode e
      (PortName srcP) = edgeSourcePort e
      (NodeId  tgtN) = edgeTargetNode e
      (PortName tgtP) = edgeTargetPort e
  in EdgeRow
    { edgeRowProgramId = toProgramRowId pid
    , edgeRowSrcNode   = srcN
    , edgeRowSrcPort   = srcP
    , edgeRowTgtNode   = tgtN
    , edgeRowTgtPort   = tgtP
    , edgeRowCategory  = encodeState (edgeCategory e)
    }

edgeFromRow :: EdgeRowId -> EdgeRow -> Either String Edge
edgeFromRow rowId row = do
  cat <- decodeState (edgeRowCategory row)
  pure Edge
    { edgeId         = fromEdgeRowId rowId
    , edgeSourceNode = NodeId   (edgeRowSrcNode row)
    , edgeSourcePort = PortName (edgeRowSrcPort row)
    , edgeTargetNode = NodeId   (edgeRowTgtNode row)
    , edgeTargetPort = PortName (edgeRowTgtPort row)
    , edgeCategory   = cat
    }

programToRow :: Program -> ProgramRow
programToRow p =
  ProgramRow
    { programRowName      = programName p
    , programRowState     = encodeState (programState p)
    , programRowCreatedAt = programCreatedAt p
    , programRowUpdatedAt = programUpdatedAt p
    }

programFromRows
  :: Entity ProgramRow
  -> [Entity NodeRow]
  -> [Entity EdgeRow]
  -> Either String Program
programFromRows (Entity pKey pRow) nodeEntities edgeEntities = do
  nodes <- traverse (\(Entity k v) -> nodeFromRow k v) nodeEntities
  edges <- traverse (\(Entity k v) -> edgeFromRow k v) edgeEntities
  st    <- decodeState (programRowState pRow)
  pure Program
    { programId        = fromProgramRowId pKey
    , programName      = programRowName pRow
    , programState     = st
    , programGraph     = Graph
        { graphNodes = Map.fromList [(nodeId n, n) | n <- nodes]
        , graphEdges = edges
        }
    , programCreatedAt = programRowCreatedAt pRow
    , programUpdatedAt = programRowUpdatedAt pRow
    }

runToRow :: Run -> RunRow
runToRow r =
  RunRow
    { runRowProgramId   = toProgramRowId (runProgramId r)
    , runRowState       = encodeState (runState r)
    , runRowTriggerInfo = fmap toJsonText (runTriggerInfo r)
    , runRowStartedAt   = runStartedAt r
    , runRowFinishedAt  = runFinishedAt r
    }

runFromRow :: Entity RunRow -> Either String Run
runFromRow (Entity k row) = do
  st <- decodeState (runRowState row)
  ti <- traverse fromJsonText (runRowTriggerInfo row)
  pure Run
    { runId          = fromRunRowId k
    , runProgramId   = fromProgramRowId (runRowProgramId row)
    , runState       = st
    , runTriggerInfo = ti
    , runStartedAt   = runRowStartedAt row
    , runFinishedAt  = runRowFinishedAt row
    }

stepToRow :: Step -> StepRow
stepToRow s =
  let (NodeId nid) = stepNodeId s
  in StepRow
    { stepRowRunId      = toRunRowId (stepRunId s)
    , stepRowNodeId     = nid
    , stepRowState      = encodeState (stepState s)
    , stepRowInput      = fmap toJsonText (stepInput s)
    , stepRowOutput     = fmap toJsonText (stepOutput s)
    , stepRowError      = stepError s
    , stepRowRetryCount = stepRetryCount s
    , stepRowStartedAt  = stepStartedAt s
    , stepRowFinishedAt = stepFinishedAt s
    }

stepFromRow :: Entity StepRow -> Either String Step
stepFromRow (Entity k row) = do
  st  <- decodeState (stepRowState row)
  inp <- traverse fromJsonText (stepRowInput row)
  out <- traverse fromJsonText (stepRowOutput row)
  pure Step
    { stepId         = fromStepRowId k
    , stepRunId      = fromRunRowId (stepRowRunId row)
    , stepNodeId     = NodeId (stepRowNodeId row)
    , stepState      = st
    , stepInput      = inp
    , stepOutput     = out
    , stepError      = stepRowError row
    , stepRetryCount = stepRowRetryCount row
    , stepStartedAt  = stepRowStartedAt row
    , stepFinishedAt = stepRowFinishedAt row
    }

-- ---------------------------------------------------------------------------
-- Programs
-- ---------------------------------------------------------------------------

insertProgram :: Program -> AppM ()
insertProgram p = runDb $ do
  insertKey (toProgramRowId (programId p)) (programToRow p)
  forM_ (Map.elems (graphNodes (programGraph p))) $ \n ->
    insertKey (toNodeRowId (nodeId n)) (nodeToRow (programId p) n)
  forM_ (graphEdges (programGraph p)) $ \e ->
    insertKey (toEdgeRowId (edgeId e)) (edgeToRow (programId p) e)

getProgram :: ProgramId -> AppM (Maybe Program)
getProgram pid = runDb $ do
  mRow <- get (toProgramRowId pid)
  case mRow of
    Nothing  -> pure Nothing
    Just row -> do
      nodes <- selectList [NodeRowProgramId ==. toProgramRowId pid] []
      edges <- selectList [EdgeRowProgramId ==. toProgramRowId pid] []
      case programFromRows (Entity (toProgramRowId pid) row) nodes edges of
        Left err -> fail $ "getProgram: " <> err
        Right p  -> pure (Just p)

listPrograms :: AppM [Program]
listPrograms = runDb $ do
  progEntities <- selectList [] [Asc ProgramRowCreatedAt]
  mapM fetchGraph progEntities
  where
    fetchGraph entity@(Entity pKey _) = do
      nodes <- selectList [NodeRowProgramId ==. pKey] []
      edges <- selectList [EdgeRowProgramId ==. pKey] []
      case programFromRows entity nodes edges of
        Left err -> fail $ "listPrograms: " <> err
        Right p  -> pure p

updateProgram :: Program -> AppM ()
updateProgram p = runDb $
  update (toProgramRowId (programId p))
    [ ProgramRowName      =. programName p
    , ProgramRowState     =. encodeState (programState p)
    , ProgramRowUpdatedAt =. programUpdatedAt p
    ]

deleteProgram :: ProgramId -> AppM ()
deleteProgram pid = runDb $ do
  deleteWhere [NodeRowProgramId ==. toProgramRowId pid]
  deleteWhere [EdgeRowProgramId ==. toProgramRowId pid]
  delete (toProgramRowId pid)

-- | Full-graph replace: delete all existing nodes and edges, insert the new
-- set. Runs in a single transaction via the pool.
putGraph :: ProgramId -> Graph -> AppM ()
putGraph pid g = runDb $ do
  deleteWhere [NodeRowProgramId ==. toProgramRowId pid]
  deleteWhere [EdgeRowProgramId ==. toProgramRowId pid]
  forM_ (Map.elems (graphNodes g)) $ \n ->
    insertKey (toNodeRowId (nodeId n)) (nodeToRow pid n)
  forM_ (graphEdges g) $ \e ->
    insertKey (toEdgeRowId (edgeId e)) (edgeToRow pid e)

-- ---------------------------------------------------------------------------
-- Runs
-- ---------------------------------------------------------------------------

insertRun :: Run -> AppM ()
insertRun r = runDb $ insertKey (toRunRowId (runId r)) (runToRow r)

getRun :: RunId -> AppM (Maybe Run)
getRun rid = runDb $ do
  mRow <- get (toRunRowId rid)
  case mRow of
    Nothing  -> pure Nothing
    Just row ->
      case runFromRow (Entity (toRunRowId rid) row) of
        Left err -> fail $ "getRun: " <> err
        Right r  -> pure (Just r)

listRunsForProgram :: ProgramId -> Int -> Int -> AppM [Run]
listRunsForProgram pid limit offset = runDb $ do
  entities <- selectList
    [RunRowProgramId ==. toProgramRowId pid]
    [Desc RunRowStartedAt, LimitTo limit, OffsetBy offset]
  traverse decodeRun entities
  where
    decodeRun e = case runFromRow e of
      Left err -> fail $ "listRunsForProgram: " <> err
      Right r  -> pure r

updateRun :: RunId -> RunState -> Maybe UTCTime -> Maybe UTCTime -> AppM ()
updateRun rid st mStarted mFinished = runDb $
  update (toRunRowId rid)
    [ RunRowState      =. encodeState st
    , RunRowStartedAt  =. mStarted
    , RunRowFinishedAt =. mFinished
    ]

-- ---------------------------------------------------------------------------
-- Steps
-- ---------------------------------------------------------------------------

insertStep :: Step -> AppM ()
insertStep s = runDb $ insertKey (toStepRowId (stepId s)) (stepToRow s)

getStep :: StepId -> AppM (Maybe Step)
getStep sid = runDb $ do
  mRow <- get (toStepRowId sid)
  case mRow of
    Nothing  -> pure Nothing
    Just row ->
      case stepFromRow (Entity (toStepRowId sid) row) of
        Left err -> fail $ "getStep: " <> err
        Right s  -> pure (Just s)

listStepsForRun :: RunId -> AppM [Step]
listStepsForRun rid = runDb $ do
  entities <- selectList [StepRowRunId ==. toRunRowId rid] [Asc StepRowId]
  traverse decodeStep entities
  where
    decodeStep e = case stepFromRow e of
      Left err -> fail $ "listStepsForRun: " <> err
      Right s  -> pure s

updateStep :: StepId -> StepState -> Maybe Text -> Maybe Value -> UTCTime -> AppM ()
updateStep sid st mErr mOut finishedAt = runDb $
  update (toStepRowId sid)
    [ StepRowState      =. encodeState st
    , StepRowError      =. mErr
    , StepRowOutput     =. fmap toJsonText mOut
    , StepRowFinishedAt =. Just finishedAt
    ]

-- | Update a step's state and all optional timestamp/payload fields.
-- Used by the state machine to persist each atomic transition, including
-- setting 'startedAt' on the first entry into 'running'.
updateStepTransition
  :: StepId
  -> StepState
  -> Maybe Text     -- ^ error message
  -> Maybe Value    -- ^ output payload
  -> Maybe UTCTime  -- ^ startedAt (set when pending→running)
  -> Maybe UTCTime  -- ^ finishedAt (set on terminal states)
  -> AppM ()
updateStepTransition sid st mErr mOut mStarted mFinished = runDb $
  update (toStepRowId sid)
    [ StepRowState      =. encodeState st
    , StepRowError      =. mErr
    , StepRowOutput     =. fmap toJsonText mOut
    , StepRowStartedAt  =. mStarted
    , StepRowFinishedAt =. mFinished
    ]

-- | Persist the current retry count on a step mid-execution.
-- Called inside the retry loop after each failed attempt, while the step
-- remains in Running state.
updateStepRetryCount :: StepId -> Int -> AppM ()
updateStepRetryCount sid count = runDb $
  update (toStepRowId sid) [StepRowRetryCount =. count]

-- ---------------------------------------------------------------------------
-- Log entries
-- ---------------------------------------------------------------------------

-- | Append a log entry for a step. Used by the retry loop to record each
-- failed attempt with its attempt number and error message.
insertLogEntry
  :: StepId
  -> Text       -- ^ log level (e.g. "warn", "error")
  -> Text       -- ^ human-readable message
  -> Maybe Value -- ^ optional structured data (e.g. attempt number, error)
  -> AppM ()
insertLogEntry sid level msg mData = do
  eid <- liftIO $ LogEntryRowKey . UUID.toText <$> nextRandom
  now <- liftIO getCurrentTime
  runDb $ insertKey eid LogEntryRow
    { logEntryRowStepId    = toStepRowId sid
    , logEntryRowLevel     = level
    , logEntryRowMessage   = msg
    , logEntryRowEntryData = fmap toJsonText mData
    , logEntryRowCreatedAt = now
    }

-- ---------------------------------------------------------------------------
-- Credentials
-- ---------------------------------------------------------------------------

credentialFromRow :: Entity CredentialRow -> Either String Credential
credentialFromRow (Entity k row) = do
  sys <- decodeState (credentialRowSystemType row)
  ct  <- case credentialRowCredType row of
           Nothing -> Right CredentialApiKey
           Just t  -> decodeState t
  pure Credential
    { credentialId        = fromCredentialRowId k
    , credentialName      = credentialRowName row
    , credentialSystem    = sys
    , credentialType      = ct
    , credentialCreatedAt = credentialRowCreatedAt row
    }

-- | Insert an encrypted credential. Takes the domain value plus the
-- pre-encrypted secret bytes (caller encrypts using 'Eva.Crypto.encrypt').
insertCredential :: Credential -> ByteString -> AppM ()
insertCredential c encBytes = runDb $
  insertKey (toCredentialRowId (credentialId c)) CredentialRow
    { credentialRowName          = credentialName c
    , credentialRowSystemType    = encodeState (credentialSystem c)
    , credentialRowCredType      = Just (encodeState (credentialType c))
    , credentialRowEncryptedData = encBytes
    , credentialRowCreatedAt     = credentialCreatedAt c
    }

listCredentials :: AppM [Credential]
listCredentials = runDb $ do
  entities <- selectList [] [Asc CredentialRowCreatedAt]
  traverse decode entities
  where
    decode e = case credentialFromRow e of
      Left err -> fail $ "listCredentials: " <> err
      Right c  -> pure c

deleteCredential :: CredentialId -> AppM ()
deleteCredential cid = runDb $ delete (toCredentialRowId cid)

-- | Look up a credential by ID and return its decrypted secret bytes.
-- Returns Left if the credential is not found or decryption fails.
getDecryptedCredential :: CredentialId -> AppM (Either String ByteString)
getDecryptedCredential cid = do
  credKey <- asks envCredentialKey
  mRow <- runDb $ get (toCredentialRowId cid)
  case mRow of
    Nothing  -> pure $ Left $ "credential not found: " <> T.unpack (let CredentialId t = cid in t)
    Just row -> pure $ Crypto.decrypt credKey (credentialRowEncryptedData row)

-- ---------------------------------------------------------------------------
-- Codebase ID helpers
-- ---------------------------------------------------------------------------

toCodebaseRowId :: CodebaseId -> CodebaseRowId
toCodebaseRowId (CodebaseId t) = CodebaseRowKey t

fromCodebaseRowId :: CodebaseRowId -> CodebaseId
fromCodebaseRowId (CodebaseRowKey t) = CodebaseId t

toCodeChangesetRowId :: CodeChangesetId -> CodeChangesetRowId
toCodeChangesetRowId (CodeChangesetId t) = CodeChangesetRowKey t

fromCodeChangesetRowId :: CodeChangesetRowId -> CodeChangesetId
fromCodeChangesetRowId (CodeChangesetRowKey t) = CodeChangesetId t

toCodeFileChangeRowId :: FileChangeId -> CodeFileChangeRowId
toCodeFileChangeRowId (FileChangeId t) = CodeFileChangeRowKey t

fromCodeFileChangeRowId :: CodeFileChangeRowId -> FileChangeId
fromCodeFileChangeRowId (CodeFileChangeRowKey t) = FileChangeId t

-- ---------------------------------------------------------------------------
-- Codebase row converters
-- ---------------------------------------------------------------------------

fileChangeFromRow :: Entity CodeFileChangeRow -> Either String FileChange
fileChangeFromRow (Entity k row) = do
  action <- decodeState (codeFileChangeRowAction row)
  status <- decodeState (codeFileChangeRowStatus row)
  pure FileChange
    { fileChangeId              = fromCodeFileChangeRowId k
    , fileChangeChangesetId     = fromCodeChangesetRowId (codeFileChangeRowChangesetId row)
    , fileChangePath            = codeFileChangeRowPath row
    , fileChangeAction          = action
    , fileChangeOriginalContent = codeFileChangeRowOriginalContent row
    , fileChangeProposedContent = codeFileChangeRowProposedContent row
    , fileChangeStatus          = status
    }

changesetFromRows :: Entity CodeChangesetRow -> [Entity CodeFileChangeRow] -> Either String CodeChangeset
changesetFromRows (Entity k row) fileRows = do
  status <- decodeState (codeChangesetRowStatus row)
  files  <- traverse fileChangeFromRow fileRows
  pure CodeChangeset
    { codeChangesetId        = fromCodeChangesetRowId k
    , codeChangesetRunId     = fromRunRowId (codeChangesetRowRunId row)
    , codeChangesetStepId    = fromStepRowId (codeChangesetRowStepId row)
    , codeChangesetStatus    = status
    , codeChangesetFiles     = files
    , codeChangesetCreatedAt = codeChangesetRowCreatedAt row
    }

-- ---------------------------------------------------------------------------
-- Codebases
-- ---------------------------------------------------------------------------

insertCodebase :: CodebaseId -> ProgramId -> Text -> UTCTime -> AppM ()
insertCodebase cbId pid path now = runDb $
  insertKey (toCodebaseRowId cbId) CodebaseRow
    { codebaseRowProgramId = toProgramRowId pid
    , codebaseRowPath      = path
    , codebaseRowCreatedAt = now
    }

getCodebase :: CodebaseId -> AppM (Maybe (CodebaseId, ProgramId, Text, UTCTime))
getCodebase cbId = runDb $ do
  mRow <- get (toCodebaseRowId cbId)
  pure $ fmap (\row -> ( cbId
                        , fromProgramRowId (codebaseRowProgramId row)
                        , codebaseRowPath row
                        , codebaseRowCreatedAt row
                        )) mRow

listCodebasesForProgram :: ProgramId -> AppM [(CodebaseId, Text, UTCTime)]
listCodebasesForProgram pid = runDb $ do
  entities <- selectList [CodebaseRowProgramId ==. toProgramRowId pid] [Asc CodebaseRowCreatedAt]
  pure $ map (\(Entity k row) -> (fromCodebaseRowId k, codebaseRowPath row, codebaseRowCreatedAt row)) entities

deleteCodebase :: CodebaseId -> AppM ()
deleteCodebase cbId = runDb $ delete (toCodebaseRowId cbId)

-- ---------------------------------------------------------------------------
-- Changesets
-- ---------------------------------------------------------------------------

listChangesetsForProgram :: ProgramId -> AppM [CodeChangeset]
listChangesetsForProgram pid = runDb $ do
  runEntities <- selectList [RunRowProgramId ==. toProgramRowId pid] []
  let runKeys = map (\(Entity k _) -> k) runEntities
  changesetEntities <- selectList [CodeChangesetRowRunId <-. runKeys] [Asc CodeChangesetRowCreatedAt]
  traverse fetchChangeset changesetEntities
  where
    fetchChangeset csEntity@(Entity csKey _) = do
      fileRows <- selectList [CodeFileChangeRowChangesetId ==. csKey] []
      case changesetFromRows csEntity fileRows of
        Left err -> fail $ "listChangesetsForProgram: " <> err
        Right cs -> pure cs

listChangesetsForRun :: RunId -> AppM [CodeChangeset]
listChangesetsForRun rid = runDb $ do
  changesetEntities <- selectList
    [CodeChangesetRowRunId ==. toRunRowId rid]
    [Asc CodeChangesetRowCreatedAt]
  traverse fetchChangeset changesetEntities
  where
    fetchChangeset csEntity@(Entity csKey _) = do
      fileRows <- selectList [CodeFileChangeRowChangesetId ==. csKey] []
      case changesetFromRows csEntity fileRows of
        Left err -> fail $ "listChangesetsForRun: " <> err
        Right cs -> pure cs

getChangeset :: CodeChangesetId -> AppM (Maybe CodeChangeset)
getChangeset csId = runDb $ do
  mRow <- get (toCodeChangesetRowId csId)
  case mRow of
    Nothing  -> pure Nothing
    Just row -> do
      let csKey = toCodeChangesetRowId csId
      fileRows <- selectList [CodeFileChangeRowChangesetId ==. csKey] []
      case changesetFromRows (Entity csKey row) fileRows of
        Left err -> fail $ "getChangeset: " <> err
        Right cs -> pure (Just cs)

updateFileChangeStatus :: FileChangeId -> FileChangeStatus -> AppM ()
updateFileChangeStatus fcId status = runDb $
  update (toCodeFileChangeRowId fcId)
    [CodeFileChangeRowStatus =. encodeState status]

updateChangesetStatus :: CodeChangesetId -> CodeChangesetStatus -> AppM ()
updateChangesetStatus csId status = runDb $
  update (toCodeChangesetRowId csId)
    [CodeChangesetRowStatus =. encodeState status]

-- | Insert a new CodeChangeset record (header row only; use 'insertFileChange' for each file).
insertChangeset :: CodeChangeset -> AppM ()
insertChangeset cs = runDb $
  insertKey (toCodeChangesetRowId (codeChangesetId cs)) CodeChangesetRow
    { codeChangesetRowRunId     = toRunRowId  (codeChangesetRunId  cs)
    , codeChangesetRowStepId    = toStepRowId (codeChangesetStepId cs)
    , codeChangesetRowStatus    = encodeState (codeChangesetStatus cs)
    , codeChangesetRowCreatedAt = codeChangesetCreatedAt cs
    }

-- | Insert a single FileChange row, associating it with an existing changeset.
insertFileChange :: FileChange -> AppM ()
insertFileChange fc = runDb $
  insertKey (toCodeFileChangeRowId (fileChangeId fc)) CodeFileChangeRow
    { codeFileChangeRowChangesetId     = toCodeChangesetRowId (fileChangeChangesetId fc)
    , codeFileChangeRowPath            = fileChangePath fc
    , codeFileChangeRowAction          = encodeState (fileChangeAction fc)
    , codeFileChangeRowOriginalContent = fileChangeOriginalContent fc
    , codeFileChangeRowProposedContent = fileChangeProposedContent fc
    , codeFileChangeRowStatus          = encodeState (fileChangeStatus fc)
    }
