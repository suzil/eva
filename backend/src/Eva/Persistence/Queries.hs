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
import Data.Time (UTCTime)
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
  )

import Eva.App (AppEnv (..), AppM)
import Eva.Core.Types
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

listRunsForProgram :: ProgramId -> AppM [Run]
listRunsForProgram pid = runDb $ do
  entities <- selectList [RunRowProgramId ==. toProgramRowId pid] [Asc RunRowId]
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
