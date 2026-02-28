{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API types and handlers for the Codebase Integration feature (P2-M4).
--
-- Exposes 15 endpoints:
--   8 codebase endpoints (connect/disconnect/list/tree/file-read/file-write/diff/refresh)
--   7 changeset endpoints (list by program, list by run, get, accept/reject file,
--                          accept-all, reject-all)
module Eva.Codebase.Api
  ( -- * Servant API types
    CodebaseByProgramAPI
  , CodebaseByIdAPI
  , ChangesetAPI

    -- * Handler combinators
  , codebaseByProgramHandlers
  , codebaseByIdHandlers
  , changesetHandlers
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Servant
import System.Exit (ExitCode (..))
import System.FilePath (isRelative, splitDirectories, (</>))
import System.Process.Typed (proc, readProcessStdout, setWorkingDir)

import Eva.Api.Types
import Eva.App (AppEnv (..), AppM, runAppM)
import Eva.Codebase.Scanner
  ( PathError (..)
  , ScanResult (..)
  , detectLanguage
  , scanDirectory
  , validatePath
  )
import Eva.Codebase.Types
import Eva.Core.Types (ProgramId (..), RunId (..))
import Eva.Persistence.Queries

-- ---------------------------------------------------------------------------
-- Servant API type definitions
-- ---------------------------------------------------------------------------

-- | Endpoints under /api/programs/:id/codebase
type CodebaseByProgramAPI =
  "api" :> "programs" :> Capture "id" Text :> "codebase" :>
    (    Get '[JSON] [CodebaseMetadata]
    :<|> ReqBody '[JSON] ConnectCodebaseReq :> PostCreated '[JSON] CodebaseMetadata
    :<|> Capture "cbId" Text :> DeleteNoContent
    )

-- | Endpoints under /api/codebase/:cbId
type CodebaseByIdAPI =
  "api" :> "codebase" :> Capture "cbId" Text :>
    (    "tree"    :> Get '[JSON] FileNode
    :<|> "file"    :> QueryParam "path" Text :> Get '[JSON] FileEntry
    :<|> "file"    :> ReqBody '[JSON] WriteFileReq :> Put '[JSON] NoContent
    :<|> "diff"    :> Get '[JSON] GitDiffResponse
    :<|> "refresh" :> Post '[JSON] CodebaseMetadata
    )

-- | All changeset endpoints
type ChangesetAPI =
       "api" :> "programs" :> Capture "id" Text :> "changesets" :> Get '[JSON] [CodeChangeset]
  :<|> "api" :> "runs" :> Capture "runId" Text :> "changesets" :> Get '[JSON] [CodeChangeset]
  :<|> "api" :> "changesets" :> Capture "id" Text :> ChangesetByIdAPI

-- | Sub-API for /api/changesets/:id
type ChangesetByIdAPI =
       Get '[JSON] CodeChangeset
  :<|> "files" :> Capture "fileId" Text :> "accept" :> Post '[JSON] FileChange
  :<|> "files" :> Capture "fileId" Text :> "reject" :> Post '[JSON] FileChange
  :<|> "accept-all" :> Post '[JSON] CodeChangeset
  :<|> "reject-all" :> Post '[JSON] CodeChangeset

-- ---------------------------------------------------------------------------
-- Codebase by-program handlers
-- ---------------------------------------------------------------------------

codebaseByProgramHandlers :: AppEnv -> Server CodebaseByProgramAPI
codebaseByProgramHandlers env rawId =
       listCodebasesH
  :<|> connectCodebaseH
  :<|> disconnectCodebaseH
  where
    run :: AppM a -> Handler a
    run = liftIO . runAppM env

    pid :: ProgramId
    pid = ProgramId rawId

    requireCodebase :: CodebaseId -> Handler (CodebaseId, ProgramId, Text, UTCTime)
    requireCodebase cbId = do
      mRow <- run (getCodebase cbId)
      case mRow of
        Nothing  -> throwError err404 { errBody = encode (ApiError "Codebase not found") }
        Just row -> pure row

    -- GET /api/programs/:id/codebase
    listCodebasesH :: Handler [CodebaseMetadata]
    listCodebasesH = do
      rows <- run (listCodebasesForProgram pid)
      fmap concat $ forM rows $ \(cbId, rawPath, createdAt) -> do
        result <- liftIO $ scanDirectory (T.unpack rawPath)
        pure $ case result of
          Left _ ->
            [CodebaseMetadata
              { codebaseMetaId            = cbId
              , codebaseMetaProgramId     = pid
              , codebaseMetaPath          = rawPath
              , codebaseMetaLanguageStats = mempty
              , codebaseMetaKeyFiles      = []
              , codebaseMetaGitBranch     = Nothing
              , codebaseMetaGitDirty      = False
              , codebaseMetaLastScannedAt = createdAt
              }]
          Right sr ->
            [scanResultToMeta cbId pid rawPath sr]

    -- POST /api/programs/:id/codebase
    connectCodebaseH :: ConnectCodebaseReq -> Handler CodebaseMetadata
    connectCodebaseH req = do
      let rawPath = T.unpack (ccrPath req)
      pathResult <- liftIO $ validatePath rawPath
      case pathResult of
        Left (PathTraversal p) ->
          throwError err400
            { errBody = encode (ApiError ("Path traversal not allowed: " <> T.pack p)) }
        Left (PathDoesNotExist p) ->
          throwError err400
            { errBody = encode (ApiError ("Path does not exist: " <> T.pack p)) }
        Right canonPath -> do
          cbId <- liftIO (CodebaseId . UUID.toText <$> nextRandom)
          now  <- liftIO getCurrentTime
          run (insertCodebase cbId pid (T.pack canonPath) now)
          scanResult <- liftIO $ scanDirectory canonPath
          case scanResult of
            Left err ->
              throwError err500
                { errBody = encode (ApiError ("Scan failed: " <> T.pack (show err))) }
            Right sr ->
              pure (scanResultToMeta cbId pid (T.pack canonPath) sr)

    -- DELETE /api/programs/:id/codebase/:cbId
    disconnectCodebaseH :: Text -> Handler NoContent
    disconnectCodebaseH rawCbId = do
      _ <- requireCodebase (CodebaseId rawCbId)
      run (deleteCodebase (CodebaseId rawCbId))
      pure NoContent

-- ---------------------------------------------------------------------------
-- Codebase by-id handlers
-- ---------------------------------------------------------------------------

codebaseByIdHandlers :: AppEnv -> Server CodebaseByIdAPI
codebaseByIdHandlers env rawCbId =
       treeH
  :<|> readFileH
  :<|> writeFileH
  :<|> diffH
  :<|> refreshH
  where
    run :: AppM a -> Handler a
    run = liftIO . runAppM env

    cbId :: CodebaseId
    cbId = CodebaseId rawCbId

    requireCodebase :: Handler (CodebaseId, ProgramId, Text, UTCTime)
    requireCodebase = do
      mRow <- run (getCodebase cbId)
      case mRow of
        Nothing  -> throwError err404 { errBody = encode (ApiError "Codebase not found") }
        Just row -> pure row

    -- GET /api/codebase/:cbId/tree
    treeH :: Handler FileNode
    treeH = do
      (_, _, rawPath, _) <- requireCodebase
      result <- liftIO $ scanDirectory (T.unpack rawPath)
      case result of
        Left err ->
          throwError err500
            { errBody = encode (ApiError ("Scan failed: " <> T.pack (show err))) }
        Right sr ->
          pure (scanResultTree sr)

    -- GET /api/codebase/:cbId/file?path=...
    readFileH :: Maybe Text -> Handler FileEntry
    readFileH Nothing =
      throwError err400 { errBody = encode (ApiError "Missing required query parameter: path") }
    readFileH (Just relPath) = do
      (_, _, rawRoot, _) <- requireCodebase
      absPath <- validateConfinedPath (T.unpack rawRoot) (T.unpack relPath)
      contentResult <- liftIO $ (Right <$> BL.readFile absPath)
        `catch` (\e -> pure (Left (e :: IOException)))
      case contentResult of
        Left err ->
          throwError err404
            { errBody = encode (ApiError ("File not found: " <> T.pack (show err))) }
        Right bs -> do
          let content = TE.decodeUtf8Lenient (BL.toStrict bs)
              lang    = detectLanguage (T.unpack relPath)
              size    = fromIntegral (BL.length bs)
          pure FileEntry
            { fileEntryPath     = relPath
            , fileEntryLanguage = lang
            , fileEntryContent  = content
            , fileEntrySize     = size
            }

    -- PUT /api/codebase/:cbId/file
    writeFileH :: WriteFileReq -> Handler NoContent
    writeFileH req = do
      (_, _, rawRoot, _) <- requireCodebase
      absPath <- validateConfinedPath (T.unpack rawRoot) (T.unpack (wfrPath req))
      writeResult <- liftIO $ (Right <$> BL.writeFile absPath (BL.fromStrict (TE.encodeUtf8 (wfrContent req))))
        `catch` (\e -> pure (Left (e :: IOException)))
      case writeResult of
        Left err ->
          throwError err500
            { errBody = encode (ApiError ("Write failed: " <> T.pack (show err))) }
        Right () ->
          pure NoContent

    -- GET /api/codebase/:cbId/diff
    diffH :: Handler GitDiffResponse
    diffH = do
      (_, _, rawPath, _) <- requireCodebase
      let dir = T.unpack rawPath
      (_, out) <- liftIO $
        readProcessStdout (setWorkingDir dir (proc "git" ["status", "--porcelain"]))
          `catch` (\(_ :: IOException) -> pure (ExitSuccess, BL.empty))
      let ls    = filter (not . T.null) $ T.lines (TE.decodeUtf8Lenient (BL.toStrict out))
          files = map parsePorcelainLine ls
      pure (GitDiffResponse files)

    -- POST /api/codebase/:cbId/refresh
    refreshH :: Handler CodebaseMetadata
    refreshH = do
      (_, pid, rawPath, _) <- requireCodebase
      result <- liftIO $ scanDirectory (T.unpack rawPath)
      case result of
        Left err ->
          throwError err500
            { errBody = encode (ApiError ("Scan failed: " <> T.pack (show err))) }
        Right sr ->
          pure (scanResultToMeta cbId pid rawPath sr)

-- ---------------------------------------------------------------------------
-- Changeset handlers
-- ---------------------------------------------------------------------------

changesetHandlers :: AppEnv -> Server ChangesetAPI
changesetHandlers env =
       listByProgramH
  :<|> listByRunH
  :<|> byIdHandlers
  where
    run :: AppM a -> Handler a
    run = liftIO . runAppM env

    -- GET /api/programs/:id/changesets
    listByProgramH :: Text -> Handler [CodeChangeset]
    listByProgramH rawId = run (listChangesetsForProgram (ProgramId rawId))

    -- GET /api/runs/:runId/changesets
    listByRunH :: Text -> Handler [CodeChangeset]
    listByRunH rawId = run (listChangesetsForRun (RunId rawId))

    -- Endpoints under /api/changesets/:id
    byIdHandlers :: Text -> Server ChangesetByIdAPI
    byIdHandlers rawId =
           getChangesetH
      :<|> acceptFileH
      :<|> rejectFileH
      :<|> acceptAllH
      :<|> rejectAllH
      where
        csId :: CodeChangesetId
        csId = CodeChangesetId rawId

        requireChangeset :: Handler CodeChangeset
        requireChangeset = do
          mCs <- run (getChangeset csId)
          case mCs of
            Nothing -> throwError err404 { errBody = encode (ApiError "Changeset not found") }
            Just cs -> pure cs

        -- GET /api/changesets/:id
        getChangesetH :: Handler CodeChangeset
        getChangesetH = requireChangeset

        -- POST /api/changesets/:id/files/:fileId/accept
        acceptFileH :: Text -> Handler FileChange
        acceptFileH rawFileId = updateFileH rawFileId FileChangeAccepted

        -- POST /api/changesets/:id/files/:fileId/reject
        rejectFileH :: Text -> Handler FileChange
        rejectFileH rawFileId = updateFileH rawFileId FileChangeRejected

        updateFileH :: Text -> FileChangeStatus -> Handler FileChange
        updateFileH rawFileId newStatus = do
          cs <- requireChangeset
          let fcId  = FileChangeId rawFileId
          case findFileChange fcId cs of
            Nothing ->
              throwError err404 { errBody = encode (ApiError "File change not found") }
            Just fc -> do
              run (updateFileChangeStatus fcId newStatus)
              pure fc { fileChangeStatus = newStatus }

        -- POST /api/changesets/:id/accept-all
        acceptAllH :: Handler CodeChangeset
        acceptAllH = updateAllH ChangesetApplied FileChangeAccepted

        -- POST /api/changesets/:id/reject-all
        rejectAllH :: Handler CodeChangeset
        rejectAllH = updateAllH ChangesetRejected FileChangeRejected

        updateAllH :: CodeChangesetStatus -> FileChangeStatus -> Handler CodeChangeset
        updateAllH csStatus fcStatus = do
          cs <- requireChangeset
          run (updateChangesetStatus csId csStatus)
          mapM_ (\fc -> run (updateFileChangeStatus (fileChangeId fc) fcStatus))
                (codeChangesetFiles cs)
          pure cs
            { codeChangesetStatus = csStatus
            , codeChangesetFiles  =
                map (\fc -> fc { fileChangeStatus = fcStatus }) (codeChangesetFiles cs)
            }

-- ---------------------------------------------------------------------------
-- Pure helpers
-- ---------------------------------------------------------------------------

-- | Build CodebaseMetadata from a ScanResult.
scanResultToMeta
  :: CodebaseId -> ProgramId -> Text -> ScanResult -> CodebaseMetadata
scanResultToMeta cbId pid path sr = CodebaseMetadata
  { codebaseMetaId            = cbId
  , codebaseMetaProgramId     = pid
  , codebaseMetaPath          = path
  , codebaseMetaLanguageStats = scanResultLangStats sr
  , codebaseMetaKeyFiles      = scanResultKeyFiles sr
  , codebaseMetaGitBranch     = scanResultGitBranch sr
  , codebaseMetaGitDirty      = scanResultGitDirty sr
  , codebaseMetaLastScannedAt = scanResultScannedAt sr
  }

-- | Validate that @rel@ (a relative path from the user) is safe and confined
-- within @root@ once resolved. Returns the absolute path or throws 400/404.
validateConfinedPath :: FilePath -> FilePath -> Handler FilePath
validateConfinedPath root rel = do
  when (not (isRelative rel)) $
    throwError err400 { errBody = encode (ApiError "Path must be relative") }
  pathResult <- liftIO $ validatePath (root </> rel)
  case pathResult of
    Left (PathTraversal p) ->
      throwError err400
        { errBody = encode (ApiError ("Path traversal not allowed: " <> T.pack p)) }
    Left (PathDoesNotExist p) ->
      throwError err404
        { errBody = encode (ApiError ("Path does not exist: " <> T.pack p)) }
    Right absPath -> do
      unless (root `isPathPrefix` absPath) $
        throwError err400
          { errBody = encode (ApiError "Path escapes codebase root") }
      pure absPath

-- | True if @prefix@ is a directory-component prefix of @path@.
isPathPrefix :: FilePath -> FilePath -> Bool
isPathPrefix prefix path =
  splitDirectories prefix `prefixOf` splitDirectories path
  where
    prefixOf [] _        = True
    prefixOf _ []        = False
    prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

-- | Parse a single line from @git status --porcelain@.
-- Lines have format: @XY PATH@ where XY is a 2-char status code.
parsePorcelainLine :: Text -> GitDiffFile
parsePorcelainLine line =
  let (xy, rest) = T.splitAt 2 line
  in GitDiffFile { gdfStatus = T.strip xy, gdfPath = T.strip rest }

-- | Find a FileChange by its ID within a changeset.
findFileChange :: FileChangeId -> CodeChangeset -> Maybe FileChange
findFileChange fcId cs =
  case filter (\fc -> fileChangeId fc == fcId) (codeChangesetFiles cs) of
    []    -> Nothing
    (x:_) -> Just x
