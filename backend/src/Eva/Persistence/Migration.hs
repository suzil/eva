{-# LANGUAGE OverloadedStrings #-}

-- | Auto-migration: runs runMigration on startup to create/update tables.
-- Safe to call on every restart — persistent only applies missing changes.
module Eva.Persistence.Migration
  ( runMigrations
  ) where

import Control.Monad.Logger (runNoLoggingT)
import Database.Persist.Sql (ConnectionPool, runMigration, runSqlPool)
import Eva.Persistence.Schema (migrateAll)

-- | Create or update all 7 tables. Idempotent: no-ops when the schema is
-- already up to date.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool = runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
