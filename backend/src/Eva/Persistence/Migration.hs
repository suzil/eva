{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Auto-migration: runs runMigration on startup to create/update tables,
-- then creates the FTS5 virtual table, sync triggers, knowledge graph tables,
-- embedding/cost tables, and idempotent column additions for knowledge_entries.
-- Safe to call on every restart — persistent only applies missing changes,
-- all DDL uses IF NOT EXISTS, and ALTER TABLE is wrapped in try/catch.
module Eva.Persistence.Migration
  ( runMigrations
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.Logger (runNoLoggingT)
import Data.Text (Text)
import Database.Persist.Sql (ConnectionPool, rawExecute, runMigration, runSqlPool)
import Eva.Persistence.Schema (migrateAll)

-- | Create or update all tables. Idempotent: no-ops when schema is already
-- up to date. After Persistent migrations, creates the FTS5 virtual table,
-- 3 sync triggers, knowledge graph tables, embedding/cost tables, and adds
-- updated_by/version columns to knowledge_entries via try/catch ALTER TABLE.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool = do
  runNoLoggingT $ runSqlPool go pool
  -- ALTER TABLE is not idempotent in SQLite (throws on duplicate column),
  -- so each statement runs in its own transaction with exceptions swallowed.
  addColumnIdempotent pool alterKnowledgeEntriesUpdatedBy
  addColumnIdempotent pool alterKnowledgeEntriesVersion
  where
    go = do
      runMigration migrateAll
      rawExecute createFtsTable []
      rawExecute createFtsInsertTrigger []
      rawExecute createFtsDeleteTrigger []
      rawExecute createFtsUpdateTrigger []
      rawExecute createRelationsTable []
      rawExecute createRelationsSourceIdx []
      rawExecute createRelationsTargetIdx []
      rawExecute createRelationsTypeIdx []
      rawExecute createRelationsCascadeDeleteTrigger []
      rawExecute createEmbeddingsTable []
      rawExecute createCostsTable []

-- | Run an ALTER TABLE statement idempotently. SQLite throws when a column
-- already exists; catching and ignoring makes re-runs safe.
addColumnIdempotent :: ConnectionPool -> Text -> IO ()
addColumnIdempotent pool stmt =
  void . try @SomeException . runNoLoggingT $
    runSqlPool (rawExecute stmt []) pool

-- | Standalone FTS5 table indexing title and content of knowledge_entries.
-- Porter stemmer + unicode61 tokenizer for multilingual support.
-- Sync is maintained entirely via the 3 triggers below.
createFtsTable :: Text
createFtsTable =
  "CREATE VIRTUAL TABLE IF NOT EXISTS knowledge_fts \
  \USING fts5(title, content, tokenize='porter unicode61')"

-- | After INSERT: add new row to FTS index.
createFtsInsertTrigger :: Text
createFtsInsertTrigger =
  "CREATE TRIGGER IF NOT EXISTS knowledge_fts_ai \
  \AFTER INSERT ON knowledge_entries BEGIN \
  \  INSERT INTO knowledge_fts(rowid, title, content) \
  \  VALUES (NEW.rowid, NEW.title, NEW.content); \
  \END"

-- | After DELETE: remove deleted row from FTS index.
createFtsDeleteTrigger :: Text
createFtsDeleteTrigger =
  "CREATE TRIGGER IF NOT EXISTS knowledge_fts_ad \
  \AFTER DELETE ON knowledge_entries BEGIN \
  \  DELETE FROM knowledge_fts WHERE rowid = OLD.rowid; \
  \END"

-- | After UPDATE: replace the old FTS entry with the new content.
createFtsUpdateTrigger :: Text
createFtsUpdateTrigger =
  "CREATE TRIGGER IF NOT EXISTS knowledge_fts_au \
  \AFTER UPDATE ON knowledge_entries BEGIN \
  \  DELETE FROM knowledge_fts WHERE rowid = OLD.rowid; \
  \  INSERT INTO knowledge_fts(rowid, title, content) \
  \  VALUES (NEW.rowid, NEW.title, NEW.content); \
  \END"

-- | knowledge_relations table: directed edges between knowledge entries.
-- FKs reference knowledge_entries but cascade is handled by trigger since
-- SQLite does not enforce FK constraints by default.
createRelationsTable :: Text
createRelationsTable =
  "CREATE TABLE IF NOT EXISTS knowledge_relations ( \
  \  id TEXT PRIMARY KEY, \
  \  source_id TEXT NOT NULL REFERENCES knowledge_entries(id), \
  \  target_id TEXT NOT NULL REFERENCES knowledge_entries(id), \
  \  relation_type TEXT NOT NULL, \
  \  confidence REAL DEFAULT 1.0, \
  \  metadata TEXT DEFAULT '{}', \
  \  created_by TEXT DEFAULT 'system', \
  \  created_at DATETIME NOT NULL, \
  \  UNIQUE(source_id, target_id, relation_type) \
  \)"

-- | Index on source_id for forward-traversal queries.
createRelationsSourceIdx :: Text
createRelationsSourceIdx =
  "CREATE INDEX IF NOT EXISTS idx_relations_source \
  \ON knowledge_relations(source_id)"

-- | Index on target_id for reverse-traversal queries.
createRelationsTargetIdx :: Text
createRelationsTargetIdx =
  "CREATE INDEX IF NOT EXISTS idx_relations_target \
  \ON knowledge_relations(target_id)"

-- | Index on relation_type for type-filtered queries.
createRelationsTypeIdx :: Text
createRelationsTypeIdx =
  "CREATE INDEX IF NOT EXISTS idx_relations_type \
  \ON knowledge_relations(relation_type)"

-- | Cascade delete: when a knowledge entry is removed, also remove all
-- relations where it appears as source or target.
createRelationsCascadeDeleteTrigger :: Text
createRelationsCascadeDeleteTrigger =
  "CREATE TRIGGER IF NOT EXISTS knowledge_relations_cascade_delete \
  \AFTER DELETE ON knowledge_entries BEGIN \
  \  DELETE FROM knowledge_relations \
  \  WHERE source_id = OLD.id OR target_id = OLD.id; \
  \END"

-- | Embedding vector storage: one row per knowledge entry, keyed by entry_id.
-- Vectors are packed Float32 arrays stored in a BLOB column. Cosine similarity
-- is computed in Haskell — no sqlite-vec extension required.
-- ON DELETE CASCADE removes embeddings when the parent entry is deleted.
createEmbeddingsTable :: Text
createEmbeddingsTable =
  "CREATE TABLE IF NOT EXISTS knowledge_embeddings ( \
  \  entry_id     TEXT    PRIMARY KEY \
  \                       REFERENCES knowledge_entries(id) ON DELETE CASCADE, \
  \  model        TEXT    NOT NULL, \
  \  dimensions   INTEGER NOT NULL, \
  \  embedding    BLOB    NOT NULL, \
  \  content_hash TEXT    NOT NULL, \
  \  created_at   DATETIME NOT NULL \
  \)"

-- | Per-embedding-call cost tracking. Every call to an embedding provider
-- (Voyage AI or OpenAI fallback) inserts a row here. Aggregate queries over
-- this table power the cost summary shown in the Knowledge Library header.
createCostsTable :: Text
createCostsTable =
  "CREATE TABLE IF NOT EXISTS knowledge_costs ( \
  \  id          TEXT    PRIMARY KEY, \
  \  operation   TEXT    NOT NULL, \
  \  model       TEXT    NOT NULL, \
  \  token_count INTEGER NOT NULL, \
  \  cost_usd    REAL    NOT NULL, \
  \  created_at  DATETIME NOT NULL \
  \)"

-- | Provenance: which agent, user, or system last wrote this entry.
-- Added via idempotent ALTER TABLE since Persistent cannot add NOT NULL
-- columns with defaults to existing tables via its migration DSL.
alterKnowledgeEntriesUpdatedBy :: Text
alterKnowledgeEntriesUpdatedBy =
  "ALTER TABLE knowledge_entries \
  \ADD COLUMN updated_by TEXT NOT NULL DEFAULT 'system'"

-- | Optimistic concurrency version counter. Agents supplying a stale version
-- on write will be rejected, preventing lost-update races.
alterKnowledgeEntriesVersion :: Text
alterKnowledgeEntriesVersion =
  "ALTER TABLE knowledge_entries \
  \ADD COLUMN version INTEGER NOT NULL DEFAULT 1"
