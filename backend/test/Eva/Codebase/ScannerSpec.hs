{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eva.Codebase.ScannerSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (createDirectory)
import qualified System.Directory as Dir
import System.IO (writeFile)

import Test.Hspec

import Eva.Codebase.Scanner
import Eva.Codebase.Types (fileNodeIsDir)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- | Create a minimal temporary git-like directory with some Haskell files.
setupSampleDir :: FilePath -> IO ()
setupSampleDir root = do
  createDirectory (root </> "src")
  writeFile (root </> "src" </> "Main.hs") "module Main where\nmain :: IO ()\nmain = putStrLn \"hello\"\n"
  writeFile (root </> "src" </> "Lib.hs") "module Lib where\nfoo :: Int\nfoo = 42\n"
  writeFile (root </> "package.json") "{\"dependencies\":{\"react\":\"^18.0.0\"},\"devDependencies\":{\"typescript\":\"^5.0.0\"}}"
  writeFile (root </> "requirements.txt") "requests==2.31.0\nnumpy>=1.24\n# a comment\n"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  -- -----------------------------------------------------------------------
  -- validatePath — adversarial inputs
  -- -----------------------------------------------------------------------
  describe "validatePath — adversarial inputs" $ do
    it "rejects ../../../etc/passwd" $ do
      result <- validatePath "../../../etc/passwd"
      result `shouldSatisfy` isLeft
      case result of
        Left (PathTraversal _) -> pure ()
        other -> expectationFailure ("Expected PathTraversal, got: " ++ show other)

    it "rejects ./../../secret" $ do
      result <- validatePath "./../../secret"
      result `shouldSatisfy` isLeft
      case result of
        Left (PathTraversal _) -> pure ()
        other -> expectationFailure ("Expected PathTraversal, got: " ++ show other)

    it "rejects foo/../bar" $ do
      result <- validatePath "foo/../bar"
      result `shouldSatisfy` isLeft
      case result of
        Left (PathTraversal _) -> pure ()
        other -> expectationFailure ("Expected PathTraversal, got: " ++ show other)

    it "rejects a path that is just .." $ do
      result <- validatePath ".."
      result `shouldSatisfy` isLeft
      case result of
        Left (PathTraversal _) -> pure ()
        other -> expectationFailure ("Expected PathTraversal, got: " ++ show other)

    it "rejects path with .. in the middle" $ do
      result <- validatePath "/tmp/../etc/passwd"
      result `shouldSatisfy` isLeft
      case result of
        Left (PathTraversal _) -> pure ()
        other -> expectationFailure ("Expected PathTraversal, got: " ++ show other)

    it "returns Right for /tmp (a real directory)" $ do
      result <- validatePath "/tmp"
      result `shouldSatisfy` isRight

    it "returns PathDoesNotExist for a nonexistent path (no ..) " $ do
      result <- validatePath "/this-path-does-not-exist-eva-test-12345"
      result `shouldSatisfy` isLeft
      case result of
        Left (PathDoesNotExist _) -> pure ()
        other -> expectationFailure ("Expected PathDoesNotExist, got: " ++ show other)

  -- -----------------------------------------------------------------------
  -- scanDirectory
  -- -----------------------------------------------------------------------
  describe "scanDirectory" $ do
    it "returns Left ScanPathError for a traversal path" $ do
      result <- scanDirectory "../../../etc"
      result `shouldSatisfy` isLeft
      case result of
        Left (ScanPathError (PathTraversal _)) -> pure ()
        other -> expectationFailure ("Expected ScanPathError PathTraversal, got: " ++ show other)

    it "returns Right ScanResult for a real directory" $
      withSystemTempDirectory "eva-scanner-test" $ \tmpDir -> do
        setupSampleDir tmpDir
        result <- scanDirectory tmpDir
        result `shouldSatisfy` isRight

    it "root node isDir = True" $
      withSystemTempDirectory "eva-scanner-test" $ \tmpDir -> do
        setupSampleDir tmpDir
        Right sr <- scanDirectory tmpDir
        fileNodeIsDir (scanResultTree sr) `shouldBe` True

    it "detects Haskell files in langStats" $
      withSystemTempDirectory "eva-scanner-test" $ \tmpDir -> do
        setupSampleDir tmpDir
        Right sr <- scanDirectory tmpDir
        Map.findWithDefault 0 "hs" (scanResultLangStats sr) `shouldSatisfy` (> 0)

    it "identifies package.json as a key file" $
      withSystemTempDirectory "eva-scanner-test" $ \tmpDir -> do
        setupSampleDir tmpDir
        Right sr <- scanDirectory tmpDir
        scanResultKeyFiles sr `shouldContain` ["package.json"]

    it "does not scan deeper than maxScanDepth" $
      withSystemTempDirectory "eva-scanner-test" $ \tmpDir -> do
        -- Build a directory 7 levels deep
        let deep = tmpDir </> "a" </> "b" </> "c" </> "d" </> "e" </> "f" </> "g"
        Dir.createDirectoryIfMissing True deep
        writeFile (deep </> "secret.hs") "module Secret where"
        Right sr <- scanDirectory tmpDir
        -- The file 7 levels deep should not appear in the tree
        let treeText = show (scanResultTree sr)
        treeText `shouldNotContain` "secret.hs"

    it "skips node_modules directory" $
      withSystemTempDirectory "eva-scanner-test" $ \tmpDir -> do
        let nm = tmpDir </> "node_modules"
        createDirectory nm
        writeFile (nm </> "lodash.js") "// lodash"
        Right sr <- scanDirectory tmpDir
        let treeText = show (scanResultTree sr)
        treeText `shouldNotContain` "lodash.js"

    it "skips .git directory" $
      withSystemTempDirectory "eva-scanner-test" $ \tmpDir -> do
        let git = tmpDir </> ".git"
        createDirectory git
        writeFile (git </> "HEAD") "ref: refs/heads/main"
        Right sr <- scanDirectory tmpDir
        let treeText = show (scanResultTree sr)
        treeText `shouldNotContain` "HEAD"

  -- -----------------------------------------------------------------------
  -- scanDirectory on the Eva backend (integration)
  -- -----------------------------------------------------------------------
  describe "scanDirectory — Eva backend integration" $ do
    it "completes in reasonable time and finds Haskell files" $ do
      result <- scanDirectory "backend/src"
      case result of
        Left err ->
          -- If backend/src doesn't exist relative to CWD, skip gracefully
          pendingWith ("Could not scan backend/src: " ++ show err)
        Right sr -> do
          Map.findWithDefault 0 "hs" (scanResultLangStats sr) `shouldSatisfy` (> 0)
          fileNodeIsDir (scanResultTree sr) `shouldBe` True

  -- -----------------------------------------------------------------------
  -- readGitMeta
  -- -----------------------------------------------------------------------
  describe "readGitMeta" $ do
    it "returns GitMeta Nothing False for a non-git directory (no exception)" $
      withSystemTempDirectory "eva-no-git" $ \tmpDir -> do
        meta <- readGitMeta tmpDir
        -- Should not throw; branch is Nothing (not a git repo)
        gitBranch meta `shouldBe` Nothing
        gitDirty  meta `shouldBe` False

    it "returns a non-empty branch name for the Eva repo itself" $ do
      meta <- readGitMeta "."
      -- As long as we're inside a git repo the branch should be Just _
      gitBranch meta `shouldSatisfy` isJust

  -- -----------------------------------------------------------------------
  -- detectLanguage
  -- -----------------------------------------------------------------------
  describe "detectLanguage" $ do
    let cases :: [(String, Text)]
        cases =
          [ ("Main.hs",        "Haskell")
          , ("App.tsx",        "TypeScript")
          , ("index.ts",       "TypeScript")
          , ("app.js",         "JavaScript")
          , ("main.rs",        "Rust")
          , ("server.go",      "Go")
          , ("script.py",      "Python")
          , ("app.rb",         "Ruby")
          , ("Main.java",      "Java")
          , ("main.c",         "C")
          , ("main.cpp",       "C++")
          , ("style.css",      "CSS")
          , ("index.html",     "HTML")
          , ("config.yaml",    "YAML")
          , ("data.json",      "JSON")
          , ("README.md",      "Markdown")
          , ("query.sql",      "SQL")
          , ("deploy.sh",      "Shell")
          , ("unknown.xyz",    "Unknown")
          , ("no-extension",   "Unknown")
          ]
    mapM_
      ( \(fname, expected) ->
          it (fname ++ " -> " ++ show expected) $
            detectLanguage fname `shouldBe` expected
      )
      cases

  -- -----------------------------------------------------------------------
  -- parseDependencies
  -- -----------------------------------------------------------------------
  describe "parseDependencies" $ do
    it "returns empty list for a directory with no manifests" $
      withSystemTempDirectory "eva-deps-empty" $ \tmpDir -> do
        deps <- parseDependencies tmpDir
        deps `shouldBe` []

    it "parses package.json dependencies" $
      withSystemTempDirectory "eva-deps-npm" $ \tmpDir -> do
        writeFile (tmpDir </> "package.json")
          "{\"dependencies\":{\"react\":\"^18.0.0\"},\"devDependencies\":{\"vitest\":\"^1.0.0\"}}"
        deps <- parseDependencies tmpDir
        let names = map depName deps
        names `shouldContain` ["react"]
        names `shouldContain` ["vitest"]

    it "parses requirements.txt" $
      withSystemTempDirectory "eva-deps-py" $ \tmpDir -> do
        writeFile (tmpDir </> "requirements.txt") "requests==2.31.0\nflask>=2.0\n# comment\n"
        deps <- parseDependencies tmpDir
        let names = map depName deps
        names `shouldContain` ["requests"]
        names `shouldContain` ["flask"]

    it "parses go.mod require directives" $
      withSystemTempDirectory "eva-deps-go" $ \tmpDir -> do
        writeFile (tmpDir </> "go.mod")
          "module example.com/myapp\n\ngo 1.21\n\nrequire github.com/gin-gonic/gin v1.9.1\n"
        deps <- parseDependencies tmpDir
        let names = map depName deps
        names `shouldContain` ["github.com/gin-gonic/gin"]

    it "ignores malformed package.json gracefully" $
      withSystemTempDirectory "eva-deps-bad-json" $ \tmpDir -> do
        writeFile (tmpDir </> "package.json") "not valid json {"
        deps <- parseDependencies tmpDir
        deps `shouldBe` []
