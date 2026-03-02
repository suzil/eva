{-# LANGUAGE OverloadedStrings #-}

-- | Auto-migration: runs runMigration on startup to create/update tables,
-- then creates the FTS5 virtual table and sync triggers for knowledge search.
-- Safe to call on every restart — persistent only applies missing changes,
-- and all DDL uses IF NOT EXISTS.
module Eva.Persistence.Migration
  ( runMigrations
  ) where

import Control.Monad.Logger (runNoLoggingT)
import Data.Text (Text)
import Database.Persist.Sql (ConnectionPool, rawExecute, runMigration, runSqlPool)
import Eva.Persistence.Schema (migrateAll)

-- | Create or update all tables. Idempotent: no-ops when schema is already
-- up to date. After Persistent migrations, creates the FTS5 virtual table
-- and 3 sync triggers if they don't already exist.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool = runNoLoggingT $ runSqlPool go pool
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
