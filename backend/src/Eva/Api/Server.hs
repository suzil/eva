{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API type definitions and handler implementations.
-- Filled out in EVA-9 (programs), EVA-15 (execution), EVA-26 (WebSocket).
module Eva.Api.Server
  ( EvaAPI
  , makeApp
  ) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Types (ResponseHeaders, methodOptions, status204)
import Network.Wai
  ( Middleware
  , mapResponseHeaders
  , requestMethod
  , responseLBS
  )
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Servant

import qualified Data.Text.Encoding as TE

import Eva.Api.Types
import Eva.Api.WebSocket (wsServerApp)
import Eva.App (AppEnv (..), AppM, runAppM)
import Eva.Core.Types
import Eva.Core.Validation (validateGraph)
import qualified Eva.Crypto as Crypto
import Eva.Engine.Runner (startRun)
import Eva.Engine.StateMachine (RunContext (..))
import Eva.Persistence.Queries

-- ---------------------------------------------------------------------------
-- API type
-- ---------------------------------------------------------------------------

type EvaAPI = HealthAPI :<|> ProgramsAPI :<|> RunsAPI :<|> CredentialsAPI

type HealthAPI =
  "api" :> "health" :> Get '[JSON] HealthResponse

type ProgramsAPI =
  "api" :> "programs" :>
    (    Get '[JSON] [Program]
    :<|> ReqBody '[JSON] CreateProgramReq :> PostCreated '[JSON] Program
    :<|> Capture "id" Text :> ProgramByIdAPI
    )

type ProgramByIdAPI =
       Get '[JSON] Program
  :<|> ReqBody '[JSON] PatchProgramReq :> Patch '[JSON] Program
  :<|> DeleteNoContent
  :<|> "graph"    :> ReqBody '[JSON] Graph    :> Put '[JSON] Program
  :<|> "validate" :> Post '[JSON] ValidateResult
  :<|> "deploy"   :> Post '[JSON] Program
  :<|> "pause"    :> Post '[JSON] Program
  :<|> "resume"   :> Post '[JSON] Program
  :<|> "runs"     :> ProgramRunsAPI

-- | Endpoints under /api/programs/:id/runs
type ProgramRunsAPI =
       QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [Run]
  :<|> ReqBody '[JSON] CreateRunReq :> PostCreated '[JSON] Run

-- | Top-level /api/runs/:id endpoints (not scoped to a program)
type RunsAPI =
  "api" :> "runs" :> Capture "id" Text :>
    (    Get '[JSON] RunDetail
    :<|> "cancel" :> Post '[JSON] Run
    )

type CredentialsAPI =
  "api" :> "credentials" :>
    (    Get '[JSON] [Credential]
    :<|> ReqBody '[JSON] CreateCredentialReq :> PostCreated '[JSON] Credential
    :<|> Capture "id" Text :> DeleteNoContent
    )

-- ---------------------------------------------------------------------------
-- Application entry point
-- ---------------------------------------------------------------------------

-- | Build the WAI 'Application'.
-- WebSocket upgrade requests (to any path) are routed to 'wsServerApp';
-- all other requests go to the Servant REST API with CORS middleware.
makeApp :: AppEnv -> Application
makeApp env =
  websocketsOr WS.defaultConnectionOptions (wsServerApp env) $
    addCors $ serve (Proxy :: Proxy EvaAPI) (evaHandlers env)

-- ---------------------------------------------------------------------------
-- CORS middleware (inline — no extra dependency)
-- ---------------------------------------------------------------------------

corsHdrs :: ResponseHeaders
corsHdrs =
  [ ("Access-Control-Allow-Origin",  "*")
  , ("Access-Control-Allow-Methods", "GET, POST, PUT, PATCH, DELETE, OPTIONS")
  , ("Access-Control-Allow-Headers", "Content-Type, Authorization")
  ]

-- | Add CORS headers to all responses; handle OPTIONS preflight with 204.
addCors :: Middleware
addCors app req sendResponse
  | requestMethod req == methodOptions =
      sendResponse $ responseLBS status204 corsHdrs ""
  | otherwise =
      app req $ sendResponse . mapResponseHeaders (corsHdrs ++)

-- ---------------------------------------------------------------------------
-- Handler wiring
-- ---------------------------------------------------------------------------

evaHandlers :: AppEnv -> Server EvaAPI
evaHandlers env =
       healthHandler
  :<|> programsHandlers env
  :<|> runsHandlers env
  :<|> credentialsHandlers env

healthHandler :: Handler HealthResponse
healthHandler = pure (HealthResponse "ok")

programsHandlers :: AppEnv -> Server ProgramsAPI
programsHandlers env =
       listProgramsH
  :<|> createProgramH
  :<|> byIdHandlers
  where
    run :: AppM a -> Handler a
    run = liftIO . runAppM env

    -- GET /api/programs
    listProgramsH :: Handler [Program]
    listProgramsH = run listPrograms

    -- POST /api/programs
    createProgramH :: CreateProgramReq -> Handler Program
    createProgramH req = do
      pid <- liftIO (ProgramId . UUID.toText <$> nextRandom)
      now <- liftIO getCurrentTime
      let prog = Program
            { programId        = pid
            , programName      = cprName req
            , programState     = Draft
            , programGraph     = Graph { graphNodes = Map.empty, graphEdges = [] }
            , programCreatedAt = now
            , programUpdatedAt = now
            }
      run (insertProgram prog)
      pure prog

    -- All endpoints under /api/programs/:id
    byIdHandlers :: Text -> Server ProgramByIdAPI
    byIdHandlers rawId =
           getProgramH
      :<|> patchProgramH
      :<|> deleteProgramH
      :<|> putGraphH
      :<|> validateH
      :<|> deployH
      :<|> pauseH
      :<|> resumeH
      :<|> listRunsH
      :<|> createRunH
      where
        pid :: ProgramId
        pid = ProgramId rawId

        requireProgram :: Handler Program
        requireProgram = do
          mp <- run (getProgram pid)
          case mp of
            Nothing -> throwError err404 { errBody = encode (ApiError "Program not found") }
            Just p  -> pure p

        -- GET /api/programs/:id
        getProgramH :: Handler Program
        getProgramH = requireProgram

        -- PATCH /api/programs/:id — only name is patchable at M1
        patchProgramH :: PatchProgramReq -> Handler Program
        patchProgramH req = do
          p   <- requireProgram
          now <- liftIO getCurrentTime
          let p' = p
                { programName      = maybe (programName p) id (pprName req)
                , programUpdatedAt = now
                }
          run (updateProgram p')
          pure p'

        -- DELETE /api/programs/:id
        deleteProgramH :: Handler NoContent
        deleteProgramH = do
          _ <- requireProgram
          run (deleteProgram pid)
          pure NoContent

        -- PUT /api/programs/:id/graph — atomic full-graph replace
        putGraphH :: Graph -> Handler Program
        putGraphH g = do
          p   <- requireProgram
          now <- liftIO getCurrentTime
          run (putGraph pid g)
          let p' = p { programGraph = g, programUpdatedAt = now }
          run (updateProgram p')
          pure p'

        -- POST /api/programs/:id/validate
        validateH :: Handler ValidateResult
        validateH = do
          p <- requireProgram
          let errs = validateGraph (programGraph p)
          pure ValidateResult
            { vrValid  = null errs
            , vrErrors = errs
            }

        -- POST /api/programs/:id/deploy
        deployH :: Handler Program
        deployH = runTransition Deploy

        -- POST /api/programs/:id/pause
        pauseH :: Handler Program
        pauseH = runTransition Pause

        -- POST /api/programs/:id/resume
        resumeH :: Handler Program
        resumeH = runTransition Resume

        runTransition :: Transition -> Handler Program
        runTransition t = do
          p   <- requireProgram
          now <- liftIO getCurrentTime
          case applyTransition t (programState p) of
            Left msg ->
              throwError err409 { errBody = encode (ApiError msg) }
            Right newState -> do
              let p' = p { programState = newState, programUpdatedAt = now }
              run (updateProgram p')
              pure p'

        -- GET /api/programs/:id/runs?limit=N&offset=M
        listRunsH :: Maybe Int -> Maybe Int -> Handler [Run]
        listRunsH mLimit mOffset = do
          _ <- requireProgram
          run (listRunsForProgram pid (fromMaybe 20 mLimit) (fromMaybe 0 mOffset))

        -- POST /api/programs/:id/runs
        createRunH :: CreateRunReq -> Handler Run
        createRunH req = do
          p <- requireProgram
          let errs = validateGraph (programGraph p)
          unless (null errs) $
            throwError err400 { errBody = encode (ValidateResult False errs) }
          ctx <- run (startRun p (crrTriggerPayload req))
          liftIO $ readTVarIO (rcRun ctx)

-- ---------------------------------------------------------------------------
-- Top-level run handlers (/api/runs/:id)
-- ---------------------------------------------------------------------------

runsHandlers :: AppEnv -> Server RunsAPI
runsHandlers env rawId = getRunDetailH :<|> cancelRunH
  where
    run :: AppM a -> Handler a
    run = liftIO . runAppM env

    rid :: RunId
    rid = RunId rawId

    requireRun :: Handler Run
    requireRun = do
      mRun <- run (getRun rid)
      case mRun of
        Nothing -> throwError err404 { errBody = encode (ApiError "Run not found") }
        Just r  -> pure r

    -- GET /api/runs/:id
    getRunDetailH :: Handler RunDetail
    getRunDetailH = do
      r     <- requireRun
      steps <- run (listStepsForRun rid)
      pure (RunDetail r steps)

    -- POST /api/runs/:id/cancel
    cancelRunH :: Handler Run
    cancelRunH = do
      r   <- requireRun
      now <- liftIO getCurrentTime
      case runState r of
        s | s `elem` [RunRunning, RunWaiting] -> do
              let r' = r { runState = RunCanceled, runFinishedAt = Just now }
              run (updateRun rid RunCanceled (runStartedAt r) (Just now))
              pure r'
        _ ->
          throwError err409
            { errBody = encode (ApiError ("Cannot cancel a " <> T.toLower (T.pack (show (runState r))) <> " run")) }

-- ---------------------------------------------------------------------------
-- Credentials handlers (/api/credentials)
-- ---------------------------------------------------------------------------

credentialsHandlers :: AppEnv -> Server CredentialsAPI
credentialsHandlers env = listH :<|> createH :<|> deleteH
  where
    run :: AppM a -> Handler a
    run = liftIO . runAppM env

    -- GET /api/credentials
    listH :: Handler [Credential]
    listH = run listCredentials

    -- POST /api/credentials
    createH :: CreateCredentialReq -> Handler Credential
    createH req = do
      cid     <- liftIO (CredentialId . UUID.toText <$> nextRandom)
      now     <- liftIO getCurrentTime
      let key     = envCredentialKey env
          secret  = TE.encodeUtf8 (ccrSecret req)
      encBytes <- liftIO (Crypto.encrypt key secret)
      let cred = Credential
            { credentialId        = cid
            , credentialName      = ccrName req
            , credentialSystem    = ccrSystem req
            , credentialType      = ccrType req
            , credentialCreatedAt = now
            }
      run (insertCredential cred encBytes)
      pure cred

    -- DELETE /api/credentials/:id
    deleteH :: Text -> Handler NoContent
    deleteH rawId = do
      run (deleteCredential (CredentialId rawId))
      pure NoContent

-- ---------------------------------------------------------------------------
-- State transition logic
-- ---------------------------------------------------------------------------

data Transition = Deploy | Pause | Resume

applyTransition :: Transition -> ProgramState -> Either Text ProgramState
applyTransition Deploy Draft  = Right Active
applyTransition Pause  Active = Right Paused
applyTransition Resume Paused = Right Active
applyTransition t      from   =
  Left $
    "Cannot " <> transitionName t
    <> " a " <> T.toLower (T.pack (show from))
    <> " program"

transitionName :: Transition -> Text
transitionName Deploy = "deploy"
transitionName Pause  = "pause"
transitionName Resume = "resume"

