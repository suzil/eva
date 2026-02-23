{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Persistent model definitions (Template Haskell): 7 tables.
-- All primary keys are Text (UUID strings). JSON columns are stored as Text.
-- The `*Row` suffix avoids clashes with domain types in Eva.Core.Types.
module Eva.Persistence.Schema where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

ProgramRow sql=programs
  Id Text sqltype=text
  name Text
  state Text
  createdAt UTCTime sql=created_at
  updatedAt UTCTime sql=updated_at
  deriving Show Eq

NodeRow sql=nodes
  Id Text sqltype=text
  programId ProgramRowId sql=program_id
  typeTag Text sql=type_tag
  config Text sqltype=text
  label Text
  posX Double sql=pos_x
  posY Double sql=pos_y
  deriving Show Eq

EdgeRow sql=edges
  Id Text sqltype=text
  programId ProgramRowId sql=program_id
  srcNode Text sql=src_node
  srcPort Text sql=src_port
  tgtNode Text sql=tgt_node
  tgtPort Text sql=tgt_port
  category Text
  deriving Show Eq

RunRow sql=runs
  Id Text sqltype=text
  programId ProgramRowId sql=program_id
  state Text
  triggerInfo Text Maybe sql=trigger_info
  startedAt UTCTime Maybe sql=started_at
  finishedAt UTCTime Maybe sql=finished_at
  deriving Show Eq

StepRow sql=steps
  Id Text sqltype=text
  runId RunRowId sql=run_id
  nodeId Text sql=node_id
  state Text
  input Text Maybe
  output Text Maybe
  error Text Maybe
  retryCount Int sql=retry_count
  startedAt UTCTime Maybe sql=started_at
  finishedAt UTCTime Maybe sql=finished_at
  deriving Show Eq

LogEntryRow sql=log_entries
  Id Text sqltype=text
  stepId StepRowId sql=step_id
  level Text
  message Text
  entryData Text Maybe sql=data
  createdAt UTCTime sql=created_at
  deriving Show Eq

CredentialRow sql=credentials
  Id Text sqltype=text
  name Text
  systemType Text sql=system_type
  credType Text Maybe sql=cred_type
  encryptedData ByteString sql=encrypted_data
  createdAt UTCTime sql=created_at
  deriving Show Eq

CodebaseRow sql=codebases
  Id Text sqltype=text
  programId ProgramRowId sql=program_id
  path Text
  createdAt UTCTime sql=created_at
  deriving Show Eq

CodeChangesetRow sql=code_changesets
  Id Text sqltype=text
  runId RunRowId sql=run_id
  stepId StepRowId sql=step_id
  status Text
  createdAt UTCTime sql=created_at
  deriving Show Eq

CodeFileChangeRow sql=code_file_changes
  Id Text sqltype=text
  changesetId CodeChangesetRowId sql=changeset_id
  path Text
  action Text
  originalContent Text Maybe sql=original_content
  proposedContent Text sql=proposed_content
  status Text
  deriving Show Eq
|]
