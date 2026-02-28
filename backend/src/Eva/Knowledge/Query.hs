{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Knowledge search and context assembly (P2-M5, EVA-79).
--
-- Provides 4 search modes over knowledge_entries + knowledge_fts:
--   1. keyword   — FTS5 BM25 (searchQueryText non-empty, no struct filters)
--   2. structured — SQL WHERE on category / source_type (empty searchQueryText)
--   3. combined  — FTS5 MATCH + struct WHERE (both text and struct filters)
--   4. path      — json_extract(metadata, '$.path') LIKE ? via 'searchByPath'
--
-- FTS5 queries use a derived-table join to avoid column-name ambiguity between
-- knowledge_entries and knowledge_fts (both expose title and content columns).
module Eva.Knowledge.Query
  ( search
  , searchByPath
  , assembleAgentContext
  ) where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql
  ( Entity (..)
  , PersistValue (..)
  , SelectOpt (..)
  , Single (..)
  , rawSql
  , selectList
  , (==.)
  )

import Eva.App (AppM)
import Eva.Core.Types (NodeId, ProgramId (..))
import Eva.Knowledge.Store (entryFromRow, listEntries)
import Eva.Knowledge.Types
import Eva.Persistence.Queries (runDb)
import Eva.Persistence.Schema

-- ---------------------------------------------------------------------------
-- Encoding helpers (mirrors Store.hs — not re-exported from there)
-- ---------------------------------------------------------------------------

toJsonText :: ToJSON a => a -> Text
toJsonText = TE.decodeUtf8 . BL.toStrict . encode

encodeState :: ToJSON a => a -> Text
encodeState = T.dropAround (== '"') . toJsonText

-- ---------------------------------------------------------------------------
-- FTS5 escaping
-- ---------------------------------------------------------------------------

-- | Wrap input as a quoted FTS5 phrase, neutralising special characters
-- (* + - ^ : ( ) etc).  Embedded double-quotes are escaped by doubling.
escapeFts :: Text -> Text
escapeFts t = "\"" <> T.replace "\"" "\"\"" t <> "\""

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Search knowledge entries.  Dispatch:
--   text + no struct filters  → keyword  (FTS5 BM25)
--   text + struct filters     → combined (FTS5 + WHERE)
--   no text (any struct)      → structured (WHERE only, confidence-ordered fallback)
search :: SearchQuery -> AppM [SearchResult]
search q
  | not (T.null (searchQueryText q)) = ftsSearch q
  | otherwise                        = structuredSearch q

-- | Find entries whose stored metadata path (json_extract(metadata, '$.path'))
-- matches the given SQLite LIKE pattern, e.g. "%.hs" or "/src/Eva/%".
searchByPath :: Text -> Maybe ProgramId -> AppM [SearchResult]
searchByPath pattern mPid = do
  let (sql, params) = buildPathQuery pattern mPid
  (rows :: [Entity KnowledgeEntryRow]) <- runDb $ rawSql sql params
  pure $ mapMaybe toResult rows

-- | Assemble a context block for injection into an agent system prompt.
-- Selects the top-3 knowledge entries for the program ranked by confidence,
-- formats them with titles and separators, and truncates to 2000 characters.
-- Returns "" when no entries exist for the program.
assembleAgentContext :: ProgramId -> Maybe NodeId -> AppM Text
assembleAgentContext pid _mNodeId = do
  entries <- listEntries pid
  let top3 = take 3 $ sortBy (comparing (Down . knowledgeEntryConfidence)) entries
  if null top3
    then pure ""
    else pure . truncateContext . formatContext $ top3

-- ---------------------------------------------------------------------------
-- FTS5 search (keyword + combined)
-- ---------------------------------------------------------------------------

ftsSearch :: SearchQuery -> AppM [SearchResult]
ftsSearch q = do
  let (sql, params) = buildFtsQuery q
  (rows :: [(Entity KnowledgeEntryRow, Single Double)]) <- runDb $ rawSql sql params
  pure $ mapMaybe (\(e, Single r) -> toResultWithScore e r) rows

-- | Build the FTS5 join query.  Uses a derived-table subquery so the outer
-- SELECT only sees knowledge_entries columns + rank (avoiding the title/content
-- column-name collision with the knowledge_fts virtual table).
--
-- Generated SQL template:
--   SELECT ??, fts_r.rank
--   FROM knowledge_entries
--   INNER JOIN (SELECT rowid, rank FROM knowledge_fts WHERE knowledge_fts MATCH ?)
--              AS fts_r ON fts_r.rowid = knowledge_entries.rowid
--   [WHERE col = ? ...]
--   ORDER BY fts_r.rank LIMIT ?
buildFtsQuery :: SearchQuery -> (Text, [PersistValue])
buildFtsQuery q =
  let escapedQ            = escapeFts (searchQueryText q)
      lim                 = fromMaybe 20 (searchQueryLimit q)
      (sfClauses, sfPrms) = structFilterClauses q
      whereClause         = toWhereClause sfClauses
      sql = "SELECT ??, fts_r.rank \
            \FROM knowledge_entries \
            \INNER JOIN \
            \  (SELECT rowid, rank FROM knowledge_fts WHERE knowledge_fts MATCH ?) AS fts_r \
            \  ON fts_r.rowid = knowledge_entries.rowid"
            <> whereClause
            <> " ORDER BY fts_r.rank LIMIT ?"
      params = [PersistText escapedQ] ++ sfPrms ++ [PersistInt64 (fromIntegral lim)]
  in (sql, params)

-- ---------------------------------------------------------------------------
-- Structured search (no FTS)
-- ---------------------------------------------------------------------------

structuredSearch :: SearchQuery -> AppM [SearchResult]
structuredSearch q = do
  let lim     = fromMaybe 20 (searchQueryLimit q)
      filters = catMaybes
        [ fmap (\(ProgramId t) -> KnowledgeEntryRowProgramId ==. Just (ProgramRowKey t))
               (searchQueryProgramId q)
        , fmap (\st  -> KnowledgeEntryRowSourceType ==. encodeState st)
               (searchQuerySourceType q)
        , fmap (\cat -> KnowledgeEntryRowCategory   ==. encodeState cat)
               (searchQueryCategory q)
        ]
  entities <- runDb $ selectList filters [Asc KnowledgeEntryRowCreatedAt, LimitTo lim]
  pure $ mapMaybe toResult entities

-- ---------------------------------------------------------------------------
-- Path search
-- ---------------------------------------------------------------------------

buildPathQuery :: Text -> Maybe ProgramId -> (Text, [PersistValue])
buildPathQuery pattern mPid =
  let (pidClauses, pidPrms) = unzip $ catMaybes
        [ fmap (\(ProgramId t) ->
            ("knowledge_entries.program_id = ?", PersistText t)) mPid ]
      whereClause = toWhereClause ("json_extract(knowledge_entries.metadata, '$.path') LIKE ?" : pidClauses)
      sql    = "SELECT ?? FROM knowledge_entries" <>
               whereClause <>
               " ORDER BY knowledge_entries.created_at ASC LIMIT 20"
      params = [PersistText pattern] ++ pidPrms
  in (sql, params)

-- ---------------------------------------------------------------------------
-- WHERE clause helpers
-- ---------------------------------------------------------------------------

-- | Build " WHERE c1 AND c2 ..." or "" from a list of column predicates.
toWhereClause :: [Text] -> Text
toWhereClause [] = ""
toWhereClause cs = " WHERE " <> T.intercalate " AND " cs

-- | Column predicates and params for optional struct filters.
structFilterClauses :: SearchQuery -> ([Text], [PersistValue])
structFilterClauses q = unzip $ catMaybes
  [ fmap (\(ProgramId t) -> ("knowledge_entries.program_id = ?",    PersistText t))
         (searchQueryProgramId q)
  , fmap (\st  -> ("knowledge_entries.source_type = ?", PersistText (encodeState st)))
         (searchQuerySourceType q)
  , fmap (\cat -> ("knowledge_entries.category = ?",    PersistText (encodeState cat)))
         (searchQueryCategory q)
  ]

-- ---------------------------------------------------------------------------
-- Row → SearchResult conversion
-- ---------------------------------------------------------------------------

toResult :: Entity KnowledgeEntryRow -> Maybe SearchResult
toResult (Entity k row) = case entryFromRow k row of
  Left _  -> Nothing
  Right e -> Just SearchResult { searchResultEntry = e, searchResultScore = 1.0 }

toResultWithScore :: Entity KnowledgeEntryRow -> Double -> Maybe SearchResult
toResultWithScore (Entity k row) score = case entryFromRow k row of
  Left _  -> Nothing
  Right e -> Just SearchResult { searchResultEntry = e, searchResultScore = score }

-- ---------------------------------------------------------------------------
-- Context formatting
-- ---------------------------------------------------------------------------

formatContext :: [KnowledgeEntry] -> Text
formatContext = T.intercalate "\n\n---\n\n" . map render
  where
    render e = "**" <> knowledgeEntryTitle e <> "**\n" <> knowledgeEntryContent e

truncateContext :: Text -> Text
truncateContext t
  | T.length t > 2000 = T.take 1997 t <> "..."
  | otherwise          = t
