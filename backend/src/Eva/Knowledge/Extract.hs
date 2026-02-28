{-# LANGUAGE OverloadedStrings #-}

-- | Orchestrator for the Auto-Knowledge extraction pipeline (P2-M5).
--
-- Dispatches to the appropriate sub-extractor based on the source type:
--   SourceCodebase -> Eva.Knowledge.Extract.Codebase (EVA-77)
--   SourceLinear   -> Eva.Knowledge.Extract.Linear   (EVA-78)
--   others         -> no-op
module Eva.Knowledge.Extract
  ( extractForSource
  ) where

import Data.Text (Text)

import Eva.App (AppM)
import Eva.Core.Types (ProgramId)
import Eva.Knowledge.Types (KnowledgeSourceType (..))
import qualified Eva.Knowledge.Extract.Codebase as Codebase
import qualified Eva.Knowledge.Extract.Linear as Linear
import qualified Data.Text as T

-- | Extract knowledge entries for a given source and insert them into the
-- knowledge store, replacing any existing non-edited entries for that source.
--
-- Sub-extractor dispatch:
--   SourceCodebase  -> Extract.Codebase.extractCodebase (EVA-77)
--   SourceLinear    -> Extract.Linear.extractLinear (EVA-78)
--   others          -> no-op
extractForSource
  :: KnowledgeSourceType
  -> Maybe Text    -- ^ Source identifier (e.g. codebase root path, Linear project ID)
  -> ProgramId
  -> AppM ()
extractForSource SourceCodebase (Just path) pid =
  Codebase.extractCodebase (T.unpack path) pid
extractForSource SourceCodebase Nothing _ =
  pure ()  -- no path provided — nothing to scan
extractForSource SourceLinear (Just credId) pid =
  Linear.extractLinear credId pid
extractForSource SourceLinear Nothing _ =
  pure ()  -- no credential provided — nothing to extract
extractForSource _ _ _ =
  pure ()  -- GitHub/Http/Manual not yet implemented
