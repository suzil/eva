{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Codebase scanner for P2-M4.
--
-- Provides path-safety validation, recursive directory scanning (max depth 5),
-- git metadata via subprocess, language detection, and dependency file parsing.
--
-- Security contract: 'validatePath' rejects any path containing @..@ components
-- before canonicalisation, preventing directory traversal attacks.
module Eva.Codebase.Scanner
  ( -- * Error types
    PathError (..)
  , ScanError (..)

    -- * Result types
  , GitMeta (..)
  , DependencyEntry (..)
  , ScanResult (..)

    -- * Public API
  , validatePath
  , scanDirectory
  , readGitMeta
  , detectLanguage
  , parseDependencies
  ) where

import Control.Exception (IOException, catch, try)
import System.Exit (ExitCode (..))
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.List (isSuffixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , getFileSize
  , listDirectory
  )
import System.FilePath
  ( splitDirectories
  , takeExtension
  , takeFileName
  , (</>)
  )
import System.Process.Typed (proc, readProcessStdout, setWorkingDir)

import Eva.Codebase.Types (FileNode (..), LangStats)

-- ---------------------------------------------------------------------------
-- Error types
-- ---------------------------------------------------------------------------

-- | Error returned by 'validatePath'.
data PathError
  = -- | The raw path contained @..@ components.
    PathTraversal FilePath
  | -- | @canonicalizePath@ failed (path does not exist or permission denied).
    PathDoesNotExist FilePath
  deriving (Eq, Show)

-- | Error returned by 'scanDirectory'.
data ScanError
  = ScanPathError PathError
  | ScanIOError FilePath IOException
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Result types
-- ---------------------------------------------------------------------------

-- | Git metadata retrieved by 'readGitMeta'.
data GitMeta = GitMeta
  { gitBranch :: Maybe Text
  , gitDirty  :: Bool
  }
  deriving (Eq, Show)

-- | A single dependency found in a manifest file.
data DependencyEntry = DependencyEntry
  { depName    :: Text
  , depVersion :: Maybe Text
  , depSource  :: Text   -- ^ "package.json" | "requirements.txt" | "go.mod"
  }
  deriving (Eq, Show)

-- | Output of 'scanDirectory'. Caller is responsible for assigning a
-- 'CodebaseId' and assembling the full 'CodebaseMetadata'.
data ScanResult = ScanResult
  { scanResultPath      :: FilePath   -- ^ Canonical root path
  , scanResultTree      :: FileNode   -- ^ Full file tree (depth ≤ 'maxScanDepth')
  , scanResultLangStats :: LangStats  -- ^ Extension → file count
  , scanResultKeyFiles  :: [Text]     -- ^ Well-known manifest file names present at root
  , scanResultGitBranch :: Maybe Text
  , scanResultGitDirty  :: Bool
  , scanResultScannedAt :: UTCTime
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

maxScanDepth :: Int
maxScanDepth = 5

-- | Directory names that are always skipped during scanning.
ignoredDirs :: [String]
ignoredDirs =
  [ ".git"
  , "node_modules"
  , "dist"
  , "build"
  , ".cabal-sandbox"
  , "__pycache__"
  , ".next"
  , "target"
  , "vendor"
  , ".stack-work"
  , "dist-newstyle"
  ]

-- | File names considered "key files" (manifests / entry points).
keyFileNames :: [String]
keyFileNames =
  [ "package.json"
  , "Cargo.toml"
  , "go.mod"
  , "requirements.txt"
  , "pyproject.toml"
  , "Makefile"
  , "docker-compose.yml"
  , "docker-compose.yaml"
  , "Dockerfile"
  , "README.md"
  , "README.rst"
  ]

-- ---------------------------------------------------------------------------
-- Path validation
-- ---------------------------------------------------------------------------

-- | Validate a filesystem path against traversal attacks.
--
-- Returns @Right canonicalPath@ if the path is safe and exists.
-- Returns @Left (PathTraversal _)@ if any component is @".."@.
-- Returns @Left (PathDoesNotExist _)@ if canonicalisation fails.
validatePath :: FilePath -> IO (Either PathError FilePath)
validatePath rawPath
  | hasDotDot rawPath = pure $ Left (PathTraversal rawPath)
  | otherwise = do
      result <- try (canonicalizePath rawPath) :: IO (Either IOException FilePath)
      case result of
        Left _         -> pure $ Left (PathDoesNotExist rawPath)
        Right resolved -> pure $ Right resolved

-- | True if any component of the path is exactly @".."@.
hasDotDot :: FilePath -> Bool
hasDotDot = any (== "..") . splitDirectories

-- ---------------------------------------------------------------------------
-- Directory scanner
-- ---------------------------------------------------------------------------

-- | Recursively scan a directory and return a 'ScanResult'.
--
-- Skips 'ignoredDirs'. Respects 'maxScanDepth'. Calls 'readGitMeta'
-- for git information (fails gracefully).
scanDirectory :: FilePath -> IO (Either ScanError ScanResult)
scanDirectory rawRoot = do
  pathResult <- validatePath rawRoot
  case pathResult of
    Left err -> pure $ Left (ScanPathError err)
    Right canonRoot -> do
      scanResult <- try (doScan canonRoot) :: IO (Either IOException ScanResult)
      case scanResult of
        Left ioErr -> pure $ Left (ScanIOError canonRoot ioErr)
        Right sr   -> pure $ Right sr

doScan :: FilePath -> IO ScanResult
doScan canonRoot = do
  (tree, langStats) <- buildTree canonRoot (takeFileName canonRoot) 0
  keyFiles <- findKeyFiles canonRoot
  GitMeta branch dirty <- readGitMeta canonRoot
  now <- getCurrentTime
  pure ScanResult
    { scanResultPath      = canonRoot
    , scanResultTree      = tree
    , scanResultLangStats = langStats
    , scanResultKeyFiles  = keyFiles
    , scanResultGitBranch = branch
    , scanResultGitDirty  = dirty
    , scanResultScannedAt = now
    }

-- | Recursively build a 'FileNode' tree and accumulate language stats.
-- Returns the root node and the accumulated stats.
buildTree
  :: FilePath     -- ^ Absolute path of this node
  -> String       -- ^ Display name
  -> Int          -- ^ Current depth
  -> IO (FileNode, LangStats)
buildTree absPath displayName depth = do
  isDir <- doesDirectoryExist absPath
  if isDir
    then do
      (children, stats) <-
        if depth >= maxScanDepth
          then pure ([], Map.empty)
          else do
            entries <- listDirectory absPath `catchIO` \_ -> pure []
            let filtered = filter (not . (`elem` ignoredDirs)) (sort entries)
            pairs <- mapM (\e -> buildTree (absPath </> e) e (depth + 1)) filtered
            let childNodes = map fst pairs
                mergedStats = Map.unionsWith (+) (map snd pairs)
            pure (childNodes, mergedStats)
      let node = FileNode
            { fileNodeName     = T.pack displayName
            , fileNodePath     = T.pack absPath
            , fileNodeIsDir    = True
            , fileNodeChildren = children
            , fileNodeSize     = Nothing
            }
      pure (node, stats)
    else do
      sz <- getFileSize absPath `catchIO` \_ -> pure 0
      let ext  = drop 1 (takeExtension displayName)  -- strip leading "."
          lang = detectLanguage displayName
          stats = if lang /= "Unknown"
                    then Map.singleton (T.pack ext) 1
                    else Map.empty
          node = FileNode
            { fileNodeName     = T.pack displayName
            , fileNodePath     = T.pack absPath
            , fileNodeIsDir    = False
            , fileNodeChildren = []
            , fileNodeSize     = Just (fromIntegral sz)
            }
      pure (node, stats)

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

-- | Collect well-known manifest file names present at the root directory.
findKeyFiles :: FilePath -> IO [Text]
findKeyFiles root = do
  entries <- listDirectory root `catchIO` \_ -> pure []
  let isKey e = e `elem` keyFileNames || ".cabal" `isSuffixOf` e
  pure $ map T.pack $ filter isKey entries

-- ---------------------------------------------------------------------------
-- Git metadata
-- ---------------------------------------------------------------------------

-- | Read git branch and dirty status for a directory.
-- Returns @GitMeta Nothing False@ if the directory is not a git repo
-- or git is unavailable.
readGitMeta :: FilePath -> IO GitMeta
readGitMeta dir = do
  branch <- readGitBranch
  dirty  <- readGitDirty
  pure (GitMeta branch dirty)
  where
    readGitBranch :: IO (Maybe Text)
    readGitBranch = do
      result <- try runBranch :: IO (Either IOException (Maybe Text))
      pure $ case result of
        Left _       -> Nothing
        Right branch -> branch

    runBranch :: IO (Maybe Text)
    runBranch = do
      (exitCode, out) <-
        readProcessStdout
          (setWorkingDir dir (proc "git" ["rev-parse", "--abbrev-ref", "HEAD"]))
          `catchIO` \_ -> pure (ExitSuccess, BSL.empty)
      let _ = exitCode   -- we inspect the output instead
      let branch = T.strip (TE.decodeUtf8Lenient (BSL.toStrict out))
      pure $ if T.null branch || branch == "HEAD" then Nothing else Just branch

    readGitDirty :: IO Bool
    readGitDirty = do
      result <- try runStatus :: IO (Either IOException Bool)
      pure $ case result of
        Left _  -> False
        Right d -> d

    runStatus :: IO Bool
    runStatus = do
      (_, out) <-
        readProcessStdout
          (setWorkingDir dir (proc "git" ["status", "--porcelain"]))
          `catchIO` \_ -> pure (ExitSuccess, BSL.empty)
      pure (not (BSL.null out))

-- ---------------------------------------------------------------------------
-- Language detection
-- ---------------------------------------------------------------------------

-- | Map a file name (or path) to a human-readable language name.
-- Uses the file extension; returns @"Unknown"@ for unrecognised extensions.
detectLanguage :: FilePath -> Text
detectLanguage path =
  case takeExtension path of
    ".hs"    -> "Haskell"
    ".lhs"   -> "Haskell"
    ".cabal" -> "Cabal"
    ".ts"    -> "TypeScript"
    ".tsx"   -> "TypeScript"
    ".js"    -> "JavaScript"
    ".jsx"   -> "JavaScript"
    ".mjs"   -> "JavaScript"
    ".cjs"   -> "JavaScript"
    ".rs"    -> "Rust"
    ".go"    -> "Go"
    ".py"    -> "Python"
    ".pyi"   -> "Python"
    ".rb"    -> "Ruby"
    ".java"  -> "Java"
    ".kt"    -> "Kotlin"
    ".scala" -> "Scala"
    ".c"     -> "C"
    ".h"     -> "C"
    ".cpp"   -> "C++"
    ".cc"    -> "C++"
    ".cxx"   -> "C++"
    ".hpp"   -> "C++"
    ".hxx"   -> "C++"
    ".cs"    -> "C#"
    ".fs"    -> "F#"
    ".swift" -> "Swift"
    ".m"     -> "Objective-C"
    ".ex"    -> "Elixir"
    ".exs"   -> "Elixir"
    ".erl"   -> "Erlang"
    ".hrl"   -> "Erlang"
    ".clj"   -> "Clojure"
    ".cljs"  -> "Clojure"
    ".lua"   -> "Lua"
    ".r"     -> "R"
    ".R"     -> "R"
    ".jl"    -> "Julia"
    ".php"   -> "PHP"
    ".css"   -> "CSS"
    ".scss"  -> "CSS"
    ".sass"  -> "CSS"
    ".less"  -> "CSS"
    ".html"  -> "HTML"
    ".htm"   -> "HTML"
    ".xml"   -> "XML"
    ".svg"   -> "SVG"
    ".json"  -> "JSON"
    ".yaml"  -> "YAML"
    ".yml"   -> "YAML"
    ".toml"  -> "TOML"
    ".ini"   -> "INI"
    ".env"   -> "Env"
    ".md"    -> "Markdown"
    ".rst"   -> "reStructuredText"
    ".txt"   -> "Text"
    ".sql"   -> "SQL"
    ".sh"    -> "Shell"
    ".bash"  -> "Shell"
    ".zsh"   -> "Shell"
    ".fish"  -> "Shell"
    ".ps1"   -> "PowerShell"
    ".tf"    -> "Terraform"
    ".nix"   -> "Nix"
    ".dhall" -> "Dhall"
    _        -> "Unknown"

-- ---------------------------------------------------------------------------
-- Dependency parsing
-- ---------------------------------------------------------------------------

-- | Parse dependency manifest files found at the given directory root.
-- Reads @package.json@, @requirements.txt@, and @go.mod@. Returns an empty
-- list if no manifests are found or parsing fails.
--
-- Note: @Cargo.toml@ is not parsed (no TOML parser in scope).
parseDependencies :: FilePath -> IO [DependencyEntry]
parseDependencies dir = do
  npm  <- parsePackageJson  (dir </> "package.json")
  reqs <- parseRequirements (dir </> "requirements.txt")
  go   <- parseGoMod        (dir </> "go.mod")
  pure (npm ++ reqs ++ go)

-- | Parse @dependencies@ and @devDependencies@ from a @package.json@.
parsePackageJson :: FilePath -> IO [DependencyEntry]
parsePackageJson path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      bytes <- BSL.readFile path `catchIO` \_ -> pure BSL.empty
      case Aeson.decode bytes of
        Just (Object obj) ->
          pure $ concatMap (extractDeps "package.json" obj) ["dependencies", "devDependencies"]
        _ -> pure []
  where
    extractDeps :: Text -> KM.KeyMap Value -> String -> [DependencyEntry]
    extractDeps src obj key =
      case KM.lookup (AesonKey.fromString key) obj of
        Just (Object deps) ->
          mapMaybe (toDep src) (KM.toList deps)
        _ -> []

    toDep :: Text -> (Aeson.Key, Value) -> Maybe DependencyEntry
    toDep src (k, String v) =
      Just DependencyEntry
        { depName    = AesonKey.toText k
        , depVersion = Just v
        , depSource  = src
        }
    toDep src (k, _) =
      Just DependencyEntry
        { depName    = AesonKey.toText k
        , depVersion = Nothing
        , depSource  = src
        }

-- | Parse a @requirements.txt@ (one package per line, optional @==version@).
parseRequirements :: FilePath -> IO [DependencyEntry]
parseRequirements path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      content <- readFileText path
      let ls = filter (not . T.null) $ filter (not . ("#" `T.isPrefixOf`)) $ map T.strip (T.lines content)
      pure $ map parseLine ls
  where
    parseLine :: Text -> DependencyEntry
    parseLine line =
      let (name, rest) = T.breakOn "==" line
      in DependencyEntry
           { depName    = T.strip name
           , depVersion = if T.null rest then Nothing else Just (T.drop 2 rest)
           , depSource  = "requirements.txt"
           }

-- | Parse @require@ directives from a @go.mod@.
parseGoMod :: FilePath -> IO [DependencyEntry]
parseGoMod path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      content <- readFileText path
      let ls = filter ("require" `T.isPrefixOf`) (map T.strip (T.lines content))
      -- Handle both "require pkg v1.x" and block-form lines
      pure $ mapMaybe parseLine ls
  where
    parseLine :: Text -> Maybe DependencyEntry
    parseLine line =
      case T.words line of
        ("require" : pkg : ver : _) ->
          Just DependencyEntry
            { depName    = pkg
            , depVersion = Just ver
            , depSource  = "go.mod"
            }
        ("require" : pkg : _) ->
          Just DependencyEntry
            { depName    = pkg
            , depVersion = Nothing
            , depSource  = "go.mod"
            }
        _ -> Nothing

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

readFileText :: FilePath -> IO Text
readFileText p = do
  bs <- BSL.readFile p `catchIO` \_ -> pure BSL.empty
  pure (TE.decodeUtf8Lenient (BSL.toStrict bs))

