{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Codebase knowledge extractor (P2-M5, EVA-77).
--
-- Scans a connected codebase root and produces 6 KnowledgeEntry rows:
--   1. File Tree          — full directory tree snapshot as JSON  (structure, 1.0)
--   2. Language Stats     — extension → file count               (metadata,  1.0)
--   3. Key Files          — manifests / entry points present      (structure, 1.0)
--   4. Dependencies       — parsed from package.json / go.mod    (metadata,  1.0)
--   5. Git Metadata       — branch, dirty flag, last 10 commits  (metadata,  1.0)
--   6. Structure Summary  — one-time LLM summary (cached)        (summary,   0.6)
--
-- Caching: the summary entry is generated exactly once per codebase root.
-- On subsequent refreshes the LLM is NOT called again unless the entry has
-- been deleted. is_edited=True entries are never deleted during refresh.
module Eva.Knowledge.Extract.Codebase
  ( extractCodebase
  ) where

import Control.Exception (IOException, catch, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.List (find)
import Data.Aeson (encode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import System.Exit (ExitCode (..))
import System.Process.Typed (proc, readProcessStdout, setWorkingDir)

import Eva.App (AppEnv (..), AppM, logMsg)
import Eva.Codebase.Scanner
  ( DependencyEntry (..)
  , ScanResult (..)
  , detectLanguage
  , parseDependencies
  , scanDirectory
  )
import Eva.Codebase.Types (FileNode (..))
import Eva.Config (LogLevel (..))
import Eva.Core.Types (ProgramId, ResponseFormat (..))
import Eva.Engine.LLM (ChatMessage (..), LLMRequest (..), LLMResponse (..), clientCall)
import Eva.Knowledge.Store
  ( deleteNonEditedBySource
  , insertEntry
  , listBySource
  )
import Eva.Knowledge.Types

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Extract 6 knowledge categories from a codebase root and store them.
--
-- Refresh semantics:
--   1. Capture any existing summary entry BEFORE deletion.
--   2. Delete all non-edited entries (structural categories + any non-edited summary).
--   3. Re-insert the 5 structural categories unconditionally.
--   4. Summary handling:
--        a. If a user-edited summary survived deletion → nothing to do.
--        b. If a previous auto-generated summary existed → re-insert from its
--           cached originalContent without calling the LLM.
--        c. No prior summary (first run, or user deleted it) → call LLM.
extractCodebase :: FilePath -> ProgramId -> AppM ()
extractCodebase root pid = do
  -- 1. Snapshot existing summary entry before deletion.
  existing <- listBySource SourceCodebase (Just (T.pack root)) pid
  let mPrevSummary = find (\e -> knowledgeEntryCategory e == CategorySummary) existing

  -- 2. Scan filesystem; fail with a clear message if the root is gone.
  scanResult <- liftIO (scanDirectory root) >>= \case
    Left err -> fail $ "extractCodebase: scan failed: " <> show err
    Right sr -> pure sr

  -- 3. Gather supplementary data.
  deps    <- liftIO (parseDependencies root)
  commits <- liftIO (readGitLog root)
  now     <- liftIO getCurrentTime

  -- 4. Remove stale auto-generated entries before re-insertion.
  deleteNonEditedBySource SourceCodebase (Just (T.pack root)) pid

  -- 5. Insert 5 structured entries (confidence 1.0).
  liftIO (fileTreeEntry  root pid scanResult now) >>= insertEntry
  liftIO (langStatsEntry root pid scanResult now) >>= insertEntry
  liftIO (keyFilesEntry  root pid scanResult now) >>= insertEntry
  liftIO (depsEntry      root pid deps now)       >>= insertEntry
  liftIO (gitMetaEntry   root pid scanResult commits now) >>= insertEntry

  -- 6. Summary (cached): pick the cheapest path available.
  case mPrevSummary of
    -- 6a. User-edited summary survived deletion unchanged — skip entirely.
    Just e | knowledgeEntryIsEdited e -> pure ()

    -- 6b. Auto-generated summary was deleted but we have its cached text.
    Just e | Just cached <- knowledgeEntryOriginalContent e -> do
      eid <- liftIO $ KnowledgeEntryId . UUID.toText <$> nextRandom
      insertEntry KnowledgeEntry
        { knowledgeEntryId              = eid
        , knowledgeEntrySourceType      = SourceCodebase
        , knowledgeEntrySourceId        = Just (T.pack root)
        , knowledgeEntryProgramId       = Just pid
        , knowledgeEntryCategory        = CategorySummary
        , knowledgeEntryTitle           = "Codebase Structure Summary"
        , knowledgeEntryContent         = cached
        , knowledgeEntryOriginalContent = Just cached
        , knowledgeEntryMetadata        = object ["model" .= ("gpt-4o" :: Text)]
        , knowledgeEntryConfidence      = 0.6
        , knowledgeEntryIsEdited        = False
        , knowledgeEntryCreatedAt       = now
        , knowledgeEntryUpdatedAt       = now
        , knowledgeEntryScannedAt       = now
        }

    -- 6c. No prior summary (first run, user deleted it, or no originalContent).
    _ -> do
      llmClient <- asks envLLMClient
      let req = summaryRequest root scanResult deps commits
      result <- liftIO (clientCall llmClient req)
      case result of
        Left err ->
          logMsg LogWarn $
            "extractCodebase: LLM summary failed (" <> T.pack (show err) <>
            "); skipping summary entry"
        Right resp -> do
          eid <- liftIO $ KnowledgeEntryId . UUID.toText <$> nextRandom
          let summaryText = llmContent resp
          insertEntry KnowledgeEntry
            { knowledgeEntryId              = eid
            , knowledgeEntrySourceType      = SourceCodebase
            , knowledgeEntrySourceId        = Just (T.pack root)
            , knowledgeEntryProgramId       = Just pid
            , knowledgeEntryCategory        = CategorySummary
            , knowledgeEntryTitle           = "Codebase Structure Summary"
            , knowledgeEntryContent         = summaryText
            , knowledgeEntryOriginalContent = Just summaryText
            , knowledgeEntryMetadata        = object ["model" .= ("gpt-4o" :: Text)]
            , knowledgeEntryConfidence      = 0.6
            , knowledgeEntryIsEdited        = False
            , knowledgeEntryCreatedAt       = now
            , knowledgeEntryUpdatedAt       = now
            , knowledgeEntryScannedAt       = now
            }

-- ---------------------------------------------------------------------------
-- Predicate helpers
-- ---------------------------------------------------------------------------

isEditedSummary :: KnowledgeEntry -> Bool
isEditedSummary e =
  knowledgeEntryCategory e == CategorySummary && knowledgeEntryIsEdited e

-- ---------------------------------------------------------------------------
-- Entry constructors
-- ---------------------------------------------------------------------------

fileTreeEntry :: FilePath -> ProgramId -> ScanResult -> UTCTime -> IO KnowledgeEntry
fileTreeEntry root pid sr now = do
  eid <- KnowledgeEntryId . UUID.toText <$> nextRandom
  let treeJson = TE.decodeUtf8Lenient . BSL.toStrict . encode . toJSON $ scanResultTree sr
      nFiles   = countFiles (scanResultTree sr)
  pure KnowledgeEntry
    { knowledgeEntryId              = eid
    , knowledgeEntrySourceType      = SourceCodebase
    , knowledgeEntrySourceId        = Just (T.pack root)
    , knowledgeEntryProgramId       = Just pid
    , knowledgeEntryCategory        = CategoryStructure
    , knowledgeEntryTitle           = "File Tree"
    , knowledgeEntryContent         = treeJson
    , knowledgeEntryOriginalContent = Just treeJson
    , knowledgeEntryMetadata        = object
        [ "root"      .= root
        , "fileCount" .= nFiles
        ]
    , knowledgeEntryConfidence      = 1.0
    , knowledgeEntryIsEdited        = False
    , knowledgeEntryCreatedAt       = now
    , knowledgeEntryUpdatedAt       = now
    , knowledgeEntryScannedAt       = now
    }

langStatsEntry :: FilePath -> ProgramId -> ScanResult -> UTCTime -> IO KnowledgeEntry
langStatsEntry root pid sr now = do
  eid <- KnowledgeEntryId . UUID.toText <$> nextRandom
  let stats   = scanResultLangStats sr
      -- Aggregate by language name (multiple extensions may map to the same lang,
      -- e.g. .ts and .tsx both become "TypeScript").
      langCounts = Map.foldlWithKey' (\acc ext n ->
          let lang = let l = detectLanguage ("x." <> T.unpack ext)
                     in if l == "Unknown" then ext else l
          in Map.insertWith (+) lang n acc
        ) Map.empty stats
      content
        | Map.null langCounts = "No recognised source files found."
        | otherwise = T.intercalate ", " $
            map (\(lang, n) -> lang <> ": " <> T.pack (show n)) $
            Map.toDescList langCounts
  pure KnowledgeEntry
    { knowledgeEntryId              = eid
    , knowledgeEntrySourceType      = SourceCodebase
    , knowledgeEntrySourceId        = Just (T.pack root)
    , knowledgeEntryProgramId       = Just pid
    , knowledgeEntryCategory        = CategoryMetadata
    , knowledgeEntryTitle           = "Language Distribution"
    , knowledgeEntryContent         = content
    , knowledgeEntryOriginalContent = Just content
    , knowledgeEntryMetadata        = toJSON stats
    , knowledgeEntryConfidence      = 1.0
    , knowledgeEntryIsEdited        = False
    , knowledgeEntryCreatedAt       = now
    , knowledgeEntryUpdatedAt       = now
    , knowledgeEntryScannedAt       = now
    }

keyFilesEntry :: FilePath -> ProgramId -> ScanResult -> UTCTime -> IO KnowledgeEntry
keyFilesEntry root pid sr now = do
  eid <- KnowledgeEntryId . UUID.toText <$> nextRandom
  let kf      = scanResultKeyFiles sr
      content
        | null kf   = "No key files found at root."
        | otherwise = T.intercalate ", " kf
  pure KnowledgeEntry
    { knowledgeEntryId              = eid
    , knowledgeEntrySourceType      = SourceCodebase
    , knowledgeEntrySourceId        = Just (T.pack root)
    , knowledgeEntryProgramId       = Just pid
    , knowledgeEntryCategory        = CategoryStructure
    , knowledgeEntryTitle           = "Key Files"
    , knowledgeEntryContent         = content
    , knowledgeEntryOriginalContent = Just content
    , knowledgeEntryMetadata        = object ["root" .= root, "files" .= kf]
    , knowledgeEntryConfidence      = 1.0
    , knowledgeEntryIsEdited        = False
    , knowledgeEntryCreatedAt       = now
    , knowledgeEntryUpdatedAt       = now
    , knowledgeEntryScannedAt       = now
    }

depsEntry :: FilePath -> ProgramId -> [DependencyEntry] -> UTCTime -> IO KnowledgeEntry
depsEntry root pid deps now = do
  eid <- KnowledgeEntryId . UUID.toText <$> nextRandom
  let content
        | null deps = "No dependencies found."
        | otherwise = T.intercalate "\n" (map formatDep deps)
  pure KnowledgeEntry
    { knowledgeEntryId              = eid
    , knowledgeEntrySourceType      = SourceCodebase
    , knowledgeEntrySourceId        = Just (T.pack root)
    , knowledgeEntryProgramId       = Just pid
    , knowledgeEntryCategory        = CategoryMetadata
    , knowledgeEntryTitle           = "Dependencies"
    , knowledgeEntryContent         = content
    , knowledgeEntryOriginalContent = Just content
    , knowledgeEntryMetadata        = toJSON (map depToJson deps)
    , knowledgeEntryConfidence      = 1.0
    , knowledgeEntryIsEdited        = False
    , knowledgeEntryCreatedAt       = now
    , knowledgeEntryUpdatedAt       = now
    , knowledgeEntryScannedAt       = now
    }
  where
    formatDep d =
      depName d <> maybe "" ("@" <>) (depVersion d) <>
      " (from " <> depSource d <> ")"
    depToJson d = object
      [ "name"    .= depName d
      , "version" .= depVersion d
      , "source"  .= depSource d
      ]

gitMetaEntry
  :: FilePath -> ProgramId -> ScanResult -> [Text] -> UTCTime -> IO KnowledgeEntry
gitMetaEntry root pid sr commits now = do
  eid <- KnowledgeEntryId . UUID.toText <$> nextRandom
  let branch  = scanResultGitBranch sr
      dirty   = scanResultGitDirty sr
      status  = if dirty then "dirty" else "clean" :: Text
      content = T.stripEnd $ T.unlines $
        [ "Branch: " <> maybe "(detached)" id branch
        , "Status: " <> status
        , "Last " <> T.pack (show (length commits)) <> " commits:"
        ] ++ map ("  " <>) commits
  pure KnowledgeEntry
    { knowledgeEntryId              = eid
    , knowledgeEntrySourceType      = SourceCodebase
    , knowledgeEntrySourceId        = Just (T.pack root)
    , knowledgeEntryProgramId       = Just pid
    , knowledgeEntryCategory        = CategoryMetadata
    , knowledgeEntryTitle           = "Git Metadata"
    , knowledgeEntryContent         = content
    , knowledgeEntryOriginalContent = Just content
    , knowledgeEntryMetadata        = object
        [ "branch"  .= branch
        , "dirty"   .= dirty
        , "commits" .= commits
        ]
    , knowledgeEntryConfidence      = 1.0
    , knowledgeEntryIsEdited        = False
    , knowledgeEntryCreatedAt       = now
    , knowledgeEntryUpdatedAt       = now
    , knowledgeEntryScannedAt       = now
    }

-- ---------------------------------------------------------------------------
-- LLM summary request
-- ---------------------------------------------------------------------------

summaryRequest :: FilePath -> ScanResult -> [DependencyEntry] -> [Text] -> LLMRequest
summaryRequest root sr deps commits = LLMRequest
  { llmModel          = "gpt-4o"
  , llmMessages       =
      [ ChatMessage "system" "You produce concise codebase summaries."
      , ChatMessage "user"   prompt
      ]
  , llmTemperature    = 0.3
  , llmMaxTokens      = Just 512
  , llmResponseFormat = ResponseText
  , llmTools          = []
  }
  where
    prompt = T.unlines
      [ "Analyse this software repository and write a 2-4 sentence plain-English summary."
      , "Be specific about the language, framework, and purpose where evident."
      , ""
      , "Root: " <> T.pack root
      , "Languages: " <> langLine
      , "Key files: " <> T.intercalate ", " (scanResultKeyFiles sr)
      , "Top dependencies: " <> T.intercalate ", "
          (map depName (take 10 deps))
      , "Git branch: " <> maybe "(unknown)" id (scanResultGitBranch sr)
      , "Recent commits:"
      ] <> T.unlines (map ("  " <>) (take 5 commits))
    langLine = T.intercalate ", " $
      map (\(ext, n) -> ext <> ":" <> T.pack (show n)) $
      Map.toList (scanResultLangStats sr)

-- ---------------------------------------------------------------------------
-- Git log helper
-- ---------------------------------------------------------------------------

-- | Read the last 10 commit one-liners from git.
-- Returns an empty list if the directory is not a git repo or git is unavailable.
readGitLog :: FilePath -> IO [Text]
readGitLog dir = do
  result <- try run :: IO (Either IOException [Text])
  pure $ case result of
    Left _     -> []
    Right ls   -> ls
  where
    run :: IO [Text]
    run = do
      (_, out) <-
        readProcessStdout
          (setWorkingDir dir (proc "git" ["log", "--oneline", "-10"]))
          `catch` (\(_ :: IOException) -> pure (ExitSuccess, BSL.empty))
      pure $ filter (not . T.null) $
             T.lines (T.strip (TE.decodeUtf8Lenient (BSL.toStrict out)))

-- ---------------------------------------------------------------------------
-- Tree helper
-- ---------------------------------------------------------------------------

countFiles :: FileNode -> Int
countFiles node
  | fileNodeIsDir node = sum (map countFiles (fileNodeChildren node))
  | otherwise          = 1
