{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Connector handler: resolves a ConnectorConfig into a live ConnectorRunner.
-- Fetches and decrypts the credential, then dispatches to the appropriate
-- system-specific runner factory. Used by the Runner to populate
-- ResourceBindings.rbConnectorRunners before Agent dispatch (EVA-33).
--
-- SystemCodebase (EVA-69): no credential required. connectorEndpoint must be
-- set to the CodebaseId of a previously connected codebase. Exposes four
-- actions: list_tree, read_file, git_diff (read-only) and write_file (stages
-- a pending CodeChangeset — never writes to disk directly).
module Eva.Engine.Handlers.Connector
  ( resolveConnectorRunner
  ) where

import Control.Exception (IOException, catch, try)
import Control.Monad.Reader (ask)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import System.Directory (canonicalizePath)
import System.Exit (ExitCode (..))
import System.FilePath (isRelative, splitDirectories, (</>))
import System.Process.Typed (proc, readProcessStdout, setWorkingDir)

import Eva.Api.WebSocket (codeChangeEvent)
import Eva.App (AppEnv, AppM, broadcastEvent, runAppM)
import Eva.Codebase.Scanner
  ( ScanResult (..)
  , scanDirectory
  , detectLanguage
  )
import Eva.Codebase.Types
import Eva.Core.Types
import qualified Eva.Integration.Linear as Linear
import Eva.Persistence.Queries
  ( getCodebase
  , getDecryptedCredential
  , insertChangeset
  , insertFileChange
  )

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Resolve a ConnectorConfig into a ConnectorRunner.
--
-- For credential-based systems (Linear, GitHub, HTTP):
--   1. Check that connectorCredentialId is set.
--   2. Fetch and decrypt the credential from the DB.
--   3. Dispatch on connectorSystem to construct the system-specific runner.
--   4. Apply connectorActionFilter (if non-empty) to limit available actions.
--
-- For SystemCodebase: credentials are not used; connectorEndpoint must hold
-- the CodebaseId of a previously connected codebase.
resolveConnectorRunner
  :: ConnectorConfig
  -> RunId
  -> StepId
  -> AppM (Either ConnectorError ConnectorRunner)
resolveConnectorRunner cfg rid sid =
  case connectorSystem cfg of
    SystemCodebase -> resolveCodebaseRunner cfg rid sid
    _              -> resolveCredentialRunner cfg

-- ---------------------------------------------------------------------------
-- Credential-based runner resolution
-- ---------------------------------------------------------------------------

resolveCredentialRunner
  :: ConnectorConfig
  -> AppM (Either ConnectorError ConnectorRunner)
resolveCredentialRunner cfg = do
  case connectorCredentialId cfg of
    Nothing ->
      pure $ Left $ ConnectorMissingCredential
        "connector node has no credential configured (set connectorCredentialId)"

    Just rawId -> do
      let cid = CredentialId rawId
      result <- getDecryptedCredential cid
      case result of
        Left err ->
          pure $ Left $ ConnectorInvalidCredential (T.pack err)

        Right secretBytes ->
          pure $ Right $ applyActionFilter cfg $ mkRunner secretBytes
  where
    mkRunner secretBytes =
      case connectorSystem cfg of
        SystemLinear   -> Linear.mkLinearRunner secretBytes cfg
        SystemGitHub   -> stubRunner "github"
        SystemHttp     -> stubRunner "http"
        SystemCodebase -> stubRunner "codebase"  -- unreachable

    stubRunner name = ConnectorRunner
      { connectorAvailableActions = pure []
      , connectorExecuteAction    = \_ _ ->
          pure $ Left $ ConnectorUnsupported $
            name <> " connector not yet implemented"
      }

-- ---------------------------------------------------------------------------
-- SystemCodebase runner
-- ---------------------------------------------------------------------------

resolveCodebaseRunner
  :: ConnectorConfig
  -> RunId
  -> StepId
  -> AppM (Either ConnectorError ConnectorRunner)
resolveCodebaseRunner cfg rid sid =
  case connectorEndpoint cfg of
    Nothing ->
      pure $ Left $ ConnectorMissingCredential
        "codebase connector requires endpoint set to a CodebaseId"
    Just cbIdText -> do
      mRow <- getCodebase (CodebaseId cbIdText)
      case mRow of
        Nothing ->
          pure $ Left $ ConnectorApiError
            ("codebase not found: " <> cbIdText)
        Just (_, _, rootPath, _) -> do
          env <- ask
          pure $ Right $ applyActionFilter cfg $
            mkCodebaseRunner env (T.unpack rootPath) rid sid

-- | Build a ConnectorRunner for a connected codebase.
mkCodebaseRunner :: AppEnv -> FilePath -> RunId -> StepId -> ConnectorRunner
mkCodebaseRunner env root rid sid = ConnectorRunner
  { connectorAvailableActions = pure codebaseActions
  , connectorExecuteAction    = \(ActionName name) args ->
      case name of
        "list_tree"  -> runListTree root
        "read_file"  -> runReadFile root args
        "git_diff"   -> runGitDiff root
        "write_file" -> runWriteFile env rid sid args
        _            -> pure $ Left $ ConnectorUnsupported
                          ("unknown codebase action: " <> name)
  }

-- ---------------------------------------------------------------------------
-- Available actions
-- ---------------------------------------------------------------------------

codebaseActions :: [ActionSpec]
codebaseActions =
  [ ActionSpec
      { actionSpecName        = "list_tree"
      , actionSpecDescription = "List the file tree of the connected codebase root."
      , actionSpecParameters  = object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object []
          , "required"   .= ([] :: [Text])
          ]
      , actionSpecReturnType  = "object"
      }
  , ActionSpec
      { actionSpecName        = "read_file"
      , actionSpecDescription =
          "Read the contents of a file by its relative path within the codebase."
      , actionSpecParameters  = object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .= ("Relative path to the file" :: Text)
                  ]
              ]
          , "required" .= (["path"] :: [Text])
          ]
      , actionSpecReturnType  = "object"
      }
  , ActionSpec
      { actionSpecName        = "git_diff"
      , actionSpecDescription =
          "Return the git working-tree status (porcelain format) for the codebase."
      , actionSpecParameters  = object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object []
          , "required"   .= ([] :: [Text])
          ]
      , actionSpecReturnType  = "object"
      }
  , ActionSpec
      { actionSpecName        = "write_file"
      , actionSpecDescription =
          "Propose writing content to a file. Creates a pending CodeChangeset " <>
          "for human review — does NOT write to disk immediately."
      , actionSpecParameters  = object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .= ("Relative path to the file to write" :: Text)
                  ]
              , "content" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .= ("Proposed file content" :: Text)
                  ]
              , "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= (["add", "modify", "delete"] :: [Text])
                  , "description" .= ("Type of file change: add, modify, or delete" :: Text)
                  ]
              ]
          , "required" .= (["path", "content"] :: [Text])
          ]
      , actionSpecReturnType  = "object"
      }
  ]

-- ---------------------------------------------------------------------------
-- Read actions
-- ---------------------------------------------------------------------------

-- | Return the file tree of the codebase root as JSON.
runListTree :: FilePath -> IO (Either ConnectorError Value)
runListTree root = do
  result <- scanDirectory root
  case result of
    Left err  -> pure $ Left $ ConnectorApiError (T.pack (show err))
    Right sr  -> pure $ Right $ toJSON (scanResultTree sr)

-- | Read a single file and return its content as a JSON object.
runReadFile :: FilePath -> Value -> IO (Either ConnectorError Value)
runReadFile root args = do
  case extractTextField "path" args of
    Nothing ->
      pure $ Left $ ConnectorApiError "read_file requires 'path' parameter"
    Just relPath -> do
      pathResult <- validateRelPath root (T.unpack relPath)
      case pathResult of
        Left msg -> pure $ Left $ ConnectorApiError msg
        Right absPath -> do
          readResult <-
            (Right <$> BL.readFile absPath)
              `catch` (\e -> pure (Left (e :: IOException)))
          case readResult of
            Left err ->
              pure $ Left $ ConnectorApiError
                ("file read error: " <> T.pack (show err))
            Right bs -> do
              let content = TE.decodeUtf8Lenient (BL.toStrict bs)
                  lang    = detectLanguage (T.unpack relPath)
                  sz      = fromIntegral (BL.length bs) :: Int
              pure $ Right $ object
                [ "path"     .= relPath
                , "language" .= lang
                , "content"  .= content
                , "size"     .= sz
                ]

-- | Return git working-tree status as a JSON object with a "files" array.
runGitDiff :: FilePath -> IO (Either ConnectorError Value)
runGitDiff root = do
  result <- try runGit :: IO (Either IOException (ExitCode, BL.ByteString))
  case result of
    Left err ->
      pure $ Left $ ConnectorApiError ("git error: " <> T.pack (show err))
    Right (_, out) ->
      let ls    = filter (not . T.null) $
                    T.lines (TE.decodeUtf8Lenient (BL.toStrict out))
          files = map parsePorcelainLine ls
      in pure $ Right $ object ["files" .= files]
  where
    runGit =
      readProcessStdout
        (setWorkingDir root (proc "git" ["status", "--porcelain"]))

    parsePorcelainLine l =
      let (xy, rest) = T.splitAt 2 l
      in object ["status" .= T.strip xy, "path" .= T.strip rest]

-- ---------------------------------------------------------------------------
-- Write action
-- ---------------------------------------------------------------------------

-- | Stage a proposed file write as a pending CodeChangeset.
-- Creates the changeset + one FileChange row in the DB, then broadcasts
-- a code_change_event on the run's WS topic. Never writes to disk.
runWriteFile
  :: AppEnv
  -> RunId
  -> StepId
  -> Value
  -> IO (Either ConnectorError Value)
runWriteFile env rid sid args =
  case (extractTextField "path" args, extractTextField "content" args) of
    (Nothing, _) ->
      pure $ Left $ ConnectorApiError "write_file requires 'path' parameter"
    (_, Nothing) ->
      pure $ Left $ ConnectorApiError "write_file requires 'content' parameter"
    (Just relPath, Just content) ->
      -- Basic path safety: reject absolute paths and .. components before
      -- staging. The full confinement check runs at accept time via the
      -- API's validateConfinedPath, but we validate here to prevent
      -- misleading traversal paths from entering the changeset table.
      let pathParts = splitDirectories (T.unpack relPath)
      in if not (isRelative (T.unpack relPath))
           then pure $ Left $ ConnectorApiError
                  "path must be relative (no leading /)"
         else if any (== "..") pathParts
           then pure $ Left $ ConnectorApiError
                  ("path traversal not allowed: " <> relPath)
         else do
      let changeAction = case extractTextField "action" args of
            Just "add"    -> FileActionAdd
            Just "delete" -> FileActionDelete
            _             -> FileActionModify
      csId <- CodeChangesetId . UUID.toText <$> nextRandom
      fcId <- FileChangeId    . UUID.toText <$> nextRandom
      now  <- getCurrentTime
      let cs = CodeChangeset
                 { codeChangesetId        = csId
                 , codeChangesetRunId     = rid
                 , codeChangesetStepId    = sid
                 , codeChangesetStatus    = ChangesetPending
                 , codeChangesetFiles     = []
                 , codeChangesetCreatedAt = now
                 }
          fc = FileChange
                 { fileChangeId              = fcId
                 , fileChangeChangesetId     = csId
                 , fileChangePath            = relPath
                 , fileChangeAction          = changeAction
                 , fileChangeOriginalContent = Nothing
                 , fileChangeProposedContent = content
                 , fileChangeStatus          = FileChangePending
                 }
      runAppM env $ do
        insertChangeset cs
        insertFileChange fc
        broadcastEvent rid (codeChangeEvent rid csId 1 now)
      pure $ Right $ object
        [ "changesetId" .= csId
        , "path"        .= relPath
        , "status"      .= ("pending" :: Text)
        ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Extract a Text value by key from a JSON Object.
extractTextField :: Text -> Value -> Maybe Text
extractTextField key (Object o) =
  case KM.lookup (AesonKey.fromText key) o of
    Just (String t) -> Just t
    _               -> Nothing
extractTextField _ _ = Nothing

-- | Validate that @rel@ is a relative path with no @..@ components, resolve
-- symlinks via 'canonicalizePath', and verify the result is still confined
-- within @root@. Returns 'Left' with an error message if any check fails.
--
-- This mirrors 'validateConfinedPath' in Eva.Codebase.Api. The IO round-trip
-- is required to detect symlink escapes (e.g. a symlink at root\/evil -> \/etc
-- that would otherwise allow reading outside the codebase root).
validateRelPath :: FilePath -> FilePath -> IO (Either Text FilePath)
validateRelPath root rel
  | not (isRelative rel) =
      pure $ Left "path must be relative (no leading /)"
  | hasDotDot rel =
      pure $ Left ("path traversal not allowed: " <> T.pack rel)
  | otherwise = do
      result <- try (canonicalizePath (root </> rel)) :: IO (Either IOException FilePath)
      case result of
        Left _ ->
          pure $ Left ("path does not exist: " <> T.pack rel)
        Right absPath ->
          if splitDirectories root `isPrefixOf` splitDirectories absPath
            then pure $ Right absPath
            else pure $ Left "path escapes codebase root"
  where
    hasDotDot = any (== "..") . splitDirectories
    isPrefixOf [] _        = True
    isPrefixOf _  []       = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- ---------------------------------------------------------------------------
-- Action filter
-- ---------------------------------------------------------------------------

-- | Wrap a ConnectorRunner so that connectorAvailableActions only returns
-- actions whose names appear in the filter list.
-- If the filter list is empty, all actions are returned unchanged.
applyActionFilter :: ConnectorConfig -> ConnectorRunner -> ConnectorRunner
applyActionFilter cfg runner =
  case connectorActionFilter cfg of
    [] -> runner
    allowed ->
      let allowedSet = map ActionName allowed
      in runner
           { connectorAvailableActions = do
               specs <- connectorAvailableActions runner
               pure [ s | s <- specs, ActionName (actionSpecName s) `elem` allowedSet ]
           }
