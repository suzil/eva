{-# LANGUAGE OverloadedStrings #-}

-- | Orchestrator for the Auto-Knowledge extraction pipeline (P2-M5, EVA-77).
--
-- Dispatches to the appropriate sub-extractor based on the source type:
--   SourceCodebase -> Eva.Knowledge.Extract.Codebase (EVA-77)
--   SourceLinear   -> Eva.Knowledge.Extract.Linear   (EVA-78, not yet implemented)
--   others         -> no-op
module Eva.Knowledge.Extract
  ( extractForSource
  ) where

import Data.Text (Text)

import Eva.App (AppM)
import Eva.Core.Types (ProgramId)
import Eva.Knowledge.Types (KnowledgeSourceType (..))
import qualified Eva.Knowledge.Extract.Codebase as Codebase
import qualified Data.Text as T

-- | Extract knowledge entries for a given source and insert them into the
-- knowledge store, replacing any existing non-edited entries for that source.
--
-- Sub-extractor dispatch:
--   SourceCodebase  -> Extract.Codebase.extractCodebase (EVA-77)
--   SourceLinear    -> Extract.Linear (EVA-78, stub — no-op for now)
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
extractForSource _ _ _ =
  pure ()  -- EVA-78 handles Linear; GitHub/Http/Manual not yet implemented
