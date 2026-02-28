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
