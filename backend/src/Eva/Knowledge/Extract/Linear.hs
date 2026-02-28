{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Linear workspace knowledge extractor (P2-M5, EVA-78).
--
-- Extracts 5 knowledge categories from a Linear workspace:
--   1. Project Inventory  — teams, projects, cycle dates    (structure, 1.0)
--   2. Workflow States    — state names and types           (metadata,  1.0)
--   3. Label Taxonomy     — label names and descriptions    (metadata,  1.0)
--   4. Member Directory   — member names and emails         (metadata,  1.0)
--   5. Recent Activity    — last 20 issues, LLM-summarized  (summary,   0.6)
--
-- Caching: the activity summary is refreshed at most once per 24 hours.
-- On re-extraction within 24h the cached summary is re-inserted without
-- calling the LLM. is_edited=True entries are never touched during refresh.
module Eva.Knowledge.Extract.Linear
  ( extractLinear
  , extractLinearWith
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (parseMaybe, withArray, withObject, (.:), (.:?))
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.IO.Unsafe (unsafePerformIO)

import Eva.App (AppEnv (..), AppM, logMsg)
import Eva.Config (LogLevel (..))
import Eva.Core.Types (CredentialId (..), ProgramId, ResponseFormat (..))
import Eva.Engine.LLM
  ( ChatMessage (..)
  , LLMClient (..)
  , LLMRequest (..)
  , LLMResponse (..)
  )
import Eva.Integration.Linear (LinearApiCall, callLinearGraphQL)
import Eva.Integration.Types (ConnectorError (..))
import Eva.Knowledge.Store
  ( deleteNonEditedBySource
  , insertEntry
  , listBySource
  )
import Eva.Knowledge.Types
import Eva.Persistence.Queries (getDecryptedCredential)

-- ---------------------------------------------------------------------------
-- Module-level TLS manager (same pattern as Eva.Integration.Linear)
-- ---------------------------------------------------------------------------

{-# NOINLINE globalLinearManager #-}
globalLinearManager :: Manager
globalLinearManager = unsafePerformIO newTlsManager

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Extract 5 knowledge categories from a Linear workspace and store them.
-- Decrypts the credential identified by 'credIdText', then delegates to
-- 'extractLinearWith'.
extractLinear :: Text -> ProgramId -> AppM ()
extractLinear credIdText pid = do
  result <- getDecryptedCredential (CredentialId credIdText)
  case result of
    Left err ->
      fail $ "extractLinear: " <> err
    Right apiKeyBytes ->
      extractLinearWith
        (callLinearGraphQL globalLinearManager apiKeyBytes)
        credIdText
        pid

-- | Injectable variant used in tests. Accepts a mock 'LinearApiCall' instead
-- of a real HTTP client.
--
-- Refresh semantics:
--   1. Capture any existing summary entry BEFORE deletion.
--   2. Fetch all 5 data sets from Linear.
--   3. Delete all non-edited entries for this source.
--   4. Re-insert 4 structured entries unconditionally.
--   5. Summary handling:
--        a. Existing user-edited summary survived deletion — skip.
--        b. Existing non-edited summary scanned within last 24h — re-insert
--           from cache without calling the LLM.
--        c. No prior summary (or stale) — call LLM for a fresh summary.
extractLinearWith :: LinearApiCall -> Text -> ProgramId -> AppM ()
extractLinearWith apiCall srcId pid = do
  -- 1. Snapshot existing summary before deletion.
  existing <- listBySource SourceLinear (Just srcId) pid
  let mPrevSummary = find (\e -> knowledgeEntryCategory e == CategorySummary) existing
  now <- liftIO getCurrentTime

  -- 2. Fetch data from Linear (errors are logged as warnings; Nothing on failure).
  projectData <- fetchOrWarn apiCall "fetchProjectInventory" projectInventoryQuery
  statesData  <- fetchOrWarn apiCall "fetchWorkflowStates"   workflowStatesQuery
  labelsData  <- fetchOrWarn apiCall "fetchIssueLabels"      issueLabelsQuery
  usersData   <- fetchOrWarn apiCall "fetchMembers"          membersQuery
  issuesData  <- fetchOrWarn apiCall "fetchRecentIssues"     recentIssuesQuery

  -- 3. Remove stale auto-generated entries.
  deleteNonEditedBySource SourceLinear (Just srcId) pid

  -- 4. Insert 4 structured entries.
  liftIO (projectInventoryEntry srcId pid projectData now) >>= insertEntry
  liftIO (workflowStatesEntry   srcId pid statesData  now) >>= insertEntry
  liftIO (labelTaxonomyEntry    srcId pid labelsData  now) >>= insertEntry
  liftIO (memberDirectoryEntry  srcId pid usersData   now) >>= insertEntry

  -- 5. Summary (with 24h cache).
  case mPrevSummary of
    -- 5a. User-edited summary survived deletion — nothing to do.
    Just e | knowledgeEntryIsEdited e -> pure ()

    -- 5b. Non-edited summary scanned within the last 24h — re-insert cached.
    Just e | cacheValid now e, Just cached <- knowledgeEntryOriginalContent e -> do
      eid <- liftIO $ KnowledgeEntryId . UUID.toText <$> nextRandom
      insertEntry KnowledgeEntry
        { knowledgeEntryId              = eid
        , knowledgeEntrySourceType      = SourceLinear
        , knowledgeEntrySourceId        = Just srcId
        , knowledgeEntryProgramId       = Just pid
        , knowledgeEntryCategory        = CategorySummary
        , knowledgeEntryTitle           = "Recent Activity Summary"
        , knowledgeEntryContent         = cached
        , knowledgeEntryOriginalContent = Just cached
        , knowledgeEntryMetadata        = object ["model" .= ("gpt-4o" :: Text)]
        , knowledgeEntryConfidence      = 0.6
        , knowledgeEntryIsEdited        = False
        , knowledgeEntryCreatedAt       = now
        , knowledgeEntryUpdatedAt       = now
        , knowledgeEntryScannedAt       = knowledgeEntryScannedAt e
        }

    -- 5c. No prior summary or cache expired — call LLM.
    _ -> do
      llmClient <- asks envLLMClient
      let req = summaryRequest issuesData
      result <- liftIO (clientCall llmClient req)
      case result of
        Left err ->
          logMsg LogWarn $
            "extractLinear: LLM summary failed (" <> T.pack (show err) <>
            "); skipping summary entry"
        Right resp -> do
          eid <- liftIO $ KnowledgeEntryId . UUID.toText <$> nextRandom
          let summaryText = llmContent resp
          insertEntry KnowledgeEntry
            { knowledgeEntryId              = eid
            , knowledgeEntrySourceType      = SourceLinear
            , knowledgeEntrySourceId        = Just srcId
            , knowledgeEntryProgramId       = Just pid
            , knowledgeEntryCategory        = CategorySummary
            , knowledgeEntryTitle           = "Recent Activity Summary"
            , knowledgeEntryContent         = summaryText
            , knowledgeEntryOriginalContent = Just summaryText
            , knowledgeEntryMetadata        = object ["model" .= ("gpt-4o" :: Text)]
            , knowledgeEntryConfidence      = 0.6
            , knowledgeEntryIsEdited        = False
            , knowledgeEntryCreatedAt       = now
            , knowledgeEntryUpdatedAt       = now
            , knowledgeEntryScannedAt       = now
            }

-- | True if the entry is not user-edited and was scanned within the last 24h.
cacheValid :: UTCTime -> KnowledgeEntry -> Bool
cacheValid now e =
  not (knowledgeEntryIsEdited e) &&
  knowledgeEntryScannedAt e >= addUTCTime (negate nominalDay) now

-- ---------------------------------------------------------------------------
-- GraphQL query strings
-- ---------------------------------------------------------------------------

projectInventoryQuery :: Text
projectInventoryQuery =
  "{ teams(first: 50) { nodes { id name \
  \    activeCycle { id name startsAt endsAt } } } \
  \  projects(first: 50) { nodes { id name state description } } }"

workflowStatesQuery :: Text
workflowStatesQuery =
  "{ workflowStates(first: 100) { nodes { id name type color \
  \    team { name } } } }"

issueLabelsQuery :: Text
issueLabelsQuery =
  "{ issueLabels(first: 100) { nodes { id name color description } } }"

membersQuery :: Text
membersQuery =
  "{ users(first: 100) { nodes { id name email displayName } } }"

recentIssuesQuery :: Text
recentIssuesQuery =
  "{ issues(first: 20, orderBy: updatedAt) { nodes { \
  \    identifier title priority \
  \    state { name } \
  \    assignee { name } } } }"

-- ---------------------------------------------------------------------------
-- Entry constructors
-- ---------------------------------------------------------------------------

projectInventoryEntry :: Text -> ProgramId -> Maybe Value -> UTCTime -> IO KnowledgeEntry
projectInventoryEntry srcId pid mVal now = do
  eid <- KnowledgeEntryId . UUID.toText <$> nextRandom
  let teams    = maybe [] parseTeams    mVal
      projects = maybe [] parseProjects mVal
      teamLine = if null teams then []
                 else ["Teams: " <> T.intercalate ", " teams]
      projLine = if null projects then []
                 else map (\(p, s) -> p <> " (" <> s <> ")") projects
      content  = T.intercalate "\n" (teamLine ++ projLine)
      contentFinal
        | T.null content = "No teams or projects found."
        | otherwise      = content
  pure KnowledgeEntry
    { knowledgeEntryId              = eid
    , knowledgeEntrySourceType      = SourceLinear
    , knowledgeEntrySourceId        = Just srcId
    , knowledgeEntryProgramId       = Just pid
    , knowledgeEntryCategory        = CategoryStructure
    , knowledgeEntryTitle           = "Project Inventory"
    , knowledgeEntryContent         = contentFinal
    , knowledgeEntryOriginalContent = Just contentFinal
    , knowledgeEntryMetadata        = object
        [ "teams"    .= teams
        , "projects" .= map fst projects
        ]
    , knowledgeEntryConfidence      = 1.0
    , knowledgeEntryIsEdited        = False
    , knowledgeEntryCreatedAt       = now
    , knowledgeEntryUpdatedAt       = now
    , knowledgeEntryScannedAt       = now
    }

workflowStatesEntry :: Text -> ProgramId -> Maybe Value -> UTCTime -> IO KnowledgeEntry
workflowStatesEntry srcId pid mVal now = do
  eid <- KnowledgeEntryId . UUID.toText <$> nextRandom
  let states  = maybe [] parseWorkflowStates mVal
      content
        | null states = "No workflow states found."
        | otherwise   = T.intercalate "\n" (map formatState states)
  pure KnowledgeEntry
    { knowledgeEntryId              = eid
    , knowledgeEntrySourceType      = SourceLinear
    , knowledgeEntrySourceId        = Just srcId
    , knowledgeEntryProgramId       = Just pid
    , knowledgeEntryCategory        = CategoryMetadata
    , knowledgeEntryTitle           = "Workflow States"
    , knowledgeEntryContent         = content
    , knowledgeEntryOriginalContent = Just content
    , knowledgeEntryMetadata        = toJSON (map (\(n,t) -> object ["name" .= n, "type" .= t]) states)
    , knowledgeEntryConfidence      = 1.0
    , knowledgeEntryIsEdited        = False
    , knowledgeEntryCreatedAt       = now
    , knowledgeEntryUpdatedAt       = now
    , knowledgeEntryScannedAt       = now
    }
  where
    formatState (name, typ) = name <> " (" <> typ <> ")"

labelTaxonomyEntry :: Text -> ProgramId -> Maybe Value -> UTCTime -> IO KnowledgeEntry
labelTaxonomyEntry srcId pid mVal now = do
  eid <- KnowledgeEntryId . UUID.toText <$> nextRandom
  let labels  = maybe [] parseLabels mVal
      content
        | null labels = "No labels found."
        | otherwise   = T.intercalate "\n" (map formatLabel labels)
  pure KnowledgeEntry
    { knowledgeEntryId              = eid
    , knowledgeEntrySourceType      = SourceLinear
    , knowledgeEntrySourceId        = Just srcId
    , knowledgeEntryProgramId       = Just pid
    , knowledgeEntryCategory        = CategoryMetadata
    , knowledgeEntryTitle           = "Label Taxonomy"
    , knowledgeEntryContent         = content
    , knowledgeEntryOriginalContent = Just content
    , knowledgeEntryMetadata        = toJSON (map (\(n,d) -> object ["name" .= n, "description" .= d]) labels)
    , knowledgeEntryConfidence      = 1.0
    , knowledgeEntryIsEdited        = False
    , knowledgeEntryCreatedAt       = now
    , knowledgeEntryUpdatedAt       = now
    , knowledgeEntryScannedAt       = now
    }
  where
    formatLabel (name, Nothing)   = name
    formatLabel (name, Just desc) = name <> ": " <> desc

memberDirectoryEntry :: Text -> ProgramId -> Maybe Value -> UTCTime -> IO KnowledgeEntry
memberDirectoryEntry srcId pid mVal now = do
  eid <- KnowledgeEntryId . UUID.toText <$> nextRandom
  let members = maybe [] parseMembers mVal
      content
        | null members = "No members found."
        | otherwise    = T.intercalate "\n" (map formatMember members)
  pure KnowledgeEntry
    { knowledgeEntryId              = eid
    , knowledgeEntrySourceType      = SourceLinear
    , knowledgeEntrySourceId        = Just srcId
    , knowledgeEntryProgramId       = Just pid
    , knowledgeEntryCategory        = CategoryMetadata
    , knowledgeEntryTitle           = "Member Directory"
    , knowledgeEntryContent         = content
    , knowledgeEntryOriginalContent = Just content
    , knowledgeEntryMetadata        = toJSON (map (\(n,e) -> object ["name" .= n, "email" .= e]) members)
    , knowledgeEntryConfidence      = 1.0
    , knowledgeEntryIsEdited        = False
    , knowledgeEntryCreatedAt       = now
    , knowledgeEntryUpdatedAt       = now
    , knowledgeEntryScannedAt       = now
    }
  where
    formatMember (name, "") = name
    formatMember (name, email) = name <> " <" <> email <> ">"

-- ---------------------------------------------------------------------------
-- GraphQL response parsers
-- ---------------------------------------------------------------------------

-- Returns node arrays via the two-level "root.field.nodes" pattern.
nodesOf :: Text -> Value -> [Value]
nodesOf key v =
  maybe [] id $
    parseMaybe (withObject "root" $ \o -> do
      field <- o .: fromText key
      withObject (T.unpack key) (\f -> f .: "nodes") field >>= \nodes ->
        withArray "nodes" (pure . foldr (:) []) nodes
      ) v

parseTeams :: Value -> [Text]
parseTeams v =
  mapMaybe (parseMaybe (withObject "team" (.: "name"))) (nodesOf "teams" v)

parseProjects :: Value -> [(Text, Text)]
parseProjects v =
  mapMaybe (\n -> parseMaybe (withObject "project" $ \p -> do
      name  <- p .: "name"
      mStat <- p .:? "state"
      pure (name, maybe "unknown" id mStat)
    ) n) (nodesOf "projects" v)

parseWorkflowStates :: Value -> [(Text, Text)]
parseWorkflowStates v =
  mapMaybe (\n -> parseMaybe (withObject "state" $ \s -> do
      name <- s .: "name"
      mTyp <- s .:? "type"
      pure (name, maybe "unknown" id mTyp)
    ) n) (nodesOf "workflowStates" v)

parseLabels :: Value -> [(Text, Maybe Text)]
parseLabels v =
  mapMaybe (\n -> parseMaybe (withObject "label" $ \l -> do
      name <- l .: "name"
      desc <- l .:? "description"
      pure (name, desc)
    ) n) (nodesOf "issueLabels" v)

parseMembers :: Value -> [(Text, Text)]
parseMembers v =
  mapMaybe (\n -> parseMaybe (withObject "user" $ \u -> do
      name  <- u .: "name"
      mMail <- u .:? "email"
      pure (name, maybe "" id mMail)
    ) n) (nodesOf "users" v)

parseIssueLines :: Maybe Value -> [Text]
parseIssueLines Nothing  = []
parseIssueLines (Just v) =
  mapMaybe (\n -> parseMaybe (withObject "issue" $ \i -> do
      ident  <- i .: "identifier"
      title  <- i .: "title"
      mState <- i .:? "state"
      stateName <- case mState of
        Nothing -> pure "unknown"
        Just sv -> withObject "state" (.: "name") sv
      pure (ident <> " " <> title <> " [" <> stateName <> "]")
    ) n) (nodesOf "issues" v)

-- ---------------------------------------------------------------------------
-- LLM summary request
-- ---------------------------------------------------------------------------

summaryRequest :: Maybe Value -> LLMRequest
summaryRequest issuesData = LLMRequest
  { llmModel          = "gpt-4o"
  , llmMessages       =
      [ ChatMessage "system" "You summarise software project activity concisely."
      , ChatMessage "user"   prompt
      ]
  , llmTemperature    = 0.3
  , llmMaxTokens      = Just 512
  , llmResponseFormat = ResponseText
  , llmTools          = []
  }
  where
    lines_ = parseIssueLines issuesData
    issueList
      | null lines_ = "No recent issues found."
      | otherwise   = T.intercalate "\n" lines_
    prompt = T.unlines
      [ "Write a 2-4 sentence plain-English summary of recent activity in this Linear workspace."
      , "Focus on patterns: what kinds of work are in progress, what has been completed recently."
      , ""
      , "Recent issues:"
      , issueList
      ]

-- ---------------------------------------------------------------------------
-- Helper: run a GraphQL query, log errors as warnings, return Nothing on failure
-- ---------------------------------------------------------------------------

fetchOrWarn
  :: LinearApiCall
  -> Text     -- ^ label for log messages
  -> Text     -- ^ GraphQL query string
  -> AppM (Maybe Value)
fetchOrWarn apiCall label query = do
  result <- liftIO (apiCall query (object []))
  case result of
    Left (ConnectorApiError err) -> do
      logMsg LogWarn $ "extractLinear: " <> label <> " failed: " <> err
      pure Nothing
    Left (ConnectorInvalidCredential err) ->
      fail $ "extractLinear: invalid credential — " <> T.unpack err
    Left (ConnectorMissingCredential err) ->
      fail $ "extractLinear: missing credential — " <> T.unpack err
    Left (ConnectorUnsupported err) -> do
      logMsg LogWarn $ "extractLinear: " <> label <> " unsupported: " <> err
      pure Nothing
    Right v -> pure (Just v)
