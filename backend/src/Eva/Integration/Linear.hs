{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Linear connector: list, create, and update issues via the Linear GraphQL API.
-- EVA-31: full implementation with three ActionSpecs.
--
-- Production entry point: 'mkLinearRunner' (called by Engine.Handlers.Connector).
-- Testable entry point:   'mkLinearRunnerWith' (takes an injectable 'LinearApiCall').
module Eva.Integration.Linear
  ( mkLinearRunner
  , mkLinearRunnerWith
  , LinearApiCall
  ) where

import Control.Exception (SomeException, try)
import Control.Monad ((>=>))
import Data.Aeson
  ( Value (..)
  , decode
  , encode
  , object
  , withObject
  , (.:)
  , (.=)
  )
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Aeson.Key (fromText)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (..)
  , Response (..)
  , httpLbs
  , parseRequest
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (statusCode)
import System.IO.Unsafe (unsafePerformIO)

import Eva.Core.Types (ConnectorConfig (..))
import Eva.Integration.Types

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Injectable HTTP call for testability: (query, variables) → result.
-- 'callLinearGraphQL' is the production implementation.
type LinearApiCall = Text -> Value -> IO (Either ConnectorError Value)

-- ---------------------------------------------------------------------------
-- Module-level TLS manager
-- ---------------------------------------------------------------------------

-- | Shared TLS manager — created once at module load (standard http-client pattern).
{-# NOINLINE globalTlsManager #-}
globalTlsManager :: Manager
globalTlsManager = unsafePerformIO newTlsManager

-- ---------------------------------------------------------------------------
-- Public constructors
-- ---------------------------------------------------------------------------

-- | Build a 'ConnectorRunner' backed by the real Linear GraphQL API.
-- Called by 'Eva.Engine.Handlers.Connector.mkRunner' with the decrypted API key.
mkLinearRunner :: ByteString -> ConnectorConfig -> ConnectorRunner
mkLinearRunner apiKey cfg =
  mkLinearRunnerWith (callLinearGraphQL globalTlsManager apiKey) cfg

-- | Build a 'ConnectorRunner' with an injectable 'LinearApiCall' — used in tests.
mkLinearRunnerWith :: LinearApiCall -> ConnectorConfig -> ConnectorRunner
mkLinearRunnerWith apiCall _cfg = ConnectorRunner
  { connectorAvailableActions = pure linearActions
  , connectorExecuteAction    = executeLinearAction apiCall
  }

-- ---------------------------------------------------------------------------
-- Action specs
-- ---------------------------------------------------------------------------

linearActions :: [ActionSpec]
linearActions =
  [ ActionSpec
      { actionSpecName        = "list_issues"
      , actionSpecDescription = "List Linear issues, optionally filtered by team, project, state, or assignee."
      , actionSpecParameters  = object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "teamId"    .= object ["type" .= ("string" :: Text), "description" .= ("Team UUID" :: Text)]
              , "projectId" .= object ["type" .= ("string" :: Text), "description" .= ("Project UUID" :: Text)]
              , "state"     .= object ["type" .= ("string" :: Text), "description" .= ("State name, e.g. 'In Progress'" :: Text)]
              , "assignee"  .= object ["type" .= ("string" :: Text), "description" .= ("Assignee display name" :: Text)]
              ]
          ]
      , actionSpecReturnType  = "array of issues"
      }
  , ActionSpec
      { actionSpecName        = "create_issue"
      , actionSpecDescription = "Create a new Linear issue in the given team."
      , actionSpecParameters  = object
          [ "type"       .= ("object" :: Text)
          , "required"   .= (["title", "teamId"] :: [Text])
          , "properties" .= object
              [ "title"       .= object ["type" .= ("string" :: Text), "description" .= ("Issue title" :: Text)]
              , "teamId"      .= object ["type" .= ("string" :: Text), "description" .= ("Team UUID to create the issue in" :: Text)]
              , "description" .= object ["type" .= ("string" :: Text), "description" .= ("Issue body (markdown)" :: Text)]
              ]
          ]
      , actionSpecReturnType  = "created issue"
      }
  , ActionSpec
      { actionSpecName        = "update_issue"
      , actionSpecDescription = "Update an existing Linear issue by its UUID."
      , actionSpecParameters  = object
          [ "type"       .= ("object" :: Text)
          , "required"   .= (["issueId"] :: [Text])
          , "properties" .= object
              [ "issueId"    .= object ["type" .= ("string" :: Text), "description" .= ("Linear issue UUID (not identifier)" :: Text)]
              , "title"      .= object ["type" .= ("string" :: Text), "description" .= ("New title" :: Text)]
              , "stateId"    .= object ["type" .= ("string" :: Text), "description" .= ("New workflow state UUID" :: Text)]
              , "assigneeId" .= object ["type" .= ("string" :: Text), "description" .= ("New assignee UUID" :: Text)]
              ]
          ]
      , actionSpecReturnType  = "updated issue"
      }
  ]

-- ---------------------------------------------------------------------------
-- Action dispatch
-- ---------------------------------------------------------------------------

executeLinearAction
  :: LinearApiCall
  -> ActionName
  -> Value
  -> IO (Either ConnectorError Value)
executeLinearAction apiCall (ActionName name) args =
  case name of
    "list_issues"  -> listIssues  apiCall args
    "create_issue" -> createIssue apiCall args
    "update_issue" -> updateIssue apiCall args
    other          -> pure $ Left $ ConnectorApiError $ "unknown Linear action: " <> other

-- ---------------------------------------------------------------------------
-- list_issues
-- ---------------------------------------------------------------------------

listIssues :: LinearApiCall -> Value -> IO (Either ConnectorError Value)
listIssues apiCall args = do
  let teamId    = lookupText "teamId"    args
      projectId = lookupText "projectId" args
      state     = lookupText "state"     args
      assignee  = lookupText "assignee"  args
      filter_   = buildIssueFilter teamId projectId state assignee
      query     = "query($filter: IssueFilter) {\
                  \  issues(filter: $filter, first: 50) {\
                  \    nodes {\
                  \      id identifier title description\
                  \      state { name }\
                  \      assignee { name email }\
                  \      priority url\
                  \    }\
                  \  }\
                  \}"
      vars = object ["filter" .= filter_]
  result <- apiCall query vars
  case result of
    Left err -> pure (Left err)
    Right v  ->
      pure $ Right $ case parseMaybe (withObject "root" (.: "issues") >=> withObject "issues" (.: "nodes")) v of
        Just nodes -> nodes
        Nothing    -> v

-- | Build a Linear IssueFilter object from optional filter fields.
buildIssueFilter :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Value
buildIssueFilter teamId projectId state assignee =
  object $ mapMaybe id
    [ fmap (\t -> "team"     .= object ["id"   .= object ["eq" .= t]]) teamId
    , fmap (\p -> "project"  .= object ["id"   .= object ["eq" .= p]]) projectId
    , fmap (\s -> "state"    .= object ["name" .= object ["eq" .= s]]) state
    , fmap (\a -> "assignee" .= object ["name" .= object ["eq" .= a]]) assignee
    ]

-- ---------------------------------------------------------------------------
-- create_issue
-- ---------------------------------------------------------------------------

createIssue :: LinearApiCall -> Value -> IO (Either ConnectorError Value)
createIssue apiCall args = do
  let mTitle  = lookupText "title"       args
      mTeamId = lookupText "teamId"      args
      mDesc   = lookupText "description" args
  case (mTitle, mTeamId) of
    (Nothing, _) -> pure $ Left $ ConnectorApiError "create_issue: 'title' is required"
    (_, Nothing) -> pure $ Left $ ConnectorApiError "create_issue: 'teamId' is required"
    (Just title, Just teamId) -> do
      let input = object $ mapMaybe id
            [ Just ("title"  .= title)
            , Just ("teamId" .= teamId)
            , fmap ("description" .=) mDesc
            ]
          query = "mutation($input: IssueCreateInput!) {\
                  \  issueCreate(input: $input) {\
                  \    success\
                  \    issue { id identifier title url }\
                  \  }\
                  \}"
          vars = object ["input" .= input]
      result <- apiCall query vars
      case result of
        Left err -> pure (Left err)
        Right v  ->
          pure $ Right $ case parseMaybe (withObject "root" (.: "issueCreate")) v of
            Just r  -> r
            Nothing -> v

-- ---------------------------------------------------------------------------
-- update_issue
-- ---------------------------------------------------------------------------

updateIssue :: LinearApiCall -> Value -> IO (Either ConnectorError Value)
updateIssue apiCall args = do
  let mIssueId  = lookupText "issueId"    args
      mTitle    = lookupText "title"      args
      mStateId  = lookupText "stateId"    args
      mAssignee = lookupText "assigneeId" args
  case mIssueId of
    Nothing      -> pure $ Left $ ConnectorApiError "update_issue: 'issueId' is required"
    Just issueId -> do
      let input = object $ mapMaybe id
            [ fmap ("title"      .=) mTitle
            , fmap ("stateId"    .=) mStateId
            , fmap ("assigneeId" .=) mAssignee
            ]
          query = "mutation($id: String!, $input: IssueUpdateInput!) {\
                  \  issueUpdate(id: $id, input: $input) {\
                  \    success\
                  \    issue { id identifier title state { name } url }\
                  \  }\
                  \}"
          vars = object ["id" .= issueId, "input" .= input]
      result <- apiCall query vars
      case result of
        Left err -> pure (Left err)
        Right v  ->
          pure $ Right $ case parseMaybe (withObject "root" (.: "issueUpdate")) v of
            Just r  -> r
            Nothing -> v

-- ---------------------------------------------------------------------------
-- HTTP: callLinearGraphQL
-- ---------------------------------------------------------------------------

-- | POST a GraphQL query to the Linear API.
-- Returns the @data@ field on success, or a 'ConnectorError' on failure.
callLinearGraphQL
  :: Manager
  -> ByteString   -- ^ Decrypted Linear API key (UTF-8)
  -> Text         -- ^ GraphQL query / mutation string
  -> Value        -- ^ Variables object
  -> IO (Either ConnectorError Value)
callLinearGraphQL mgr apiKey query vars = do
  result <- try (doGraphQL mgr apiKey query vars)
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ ConnectorApiError $ "HTTP exception: " <> T.pack (show e)
    Right v -> pure v

doGraphQL
  :: Manager
  -> ByteString
  -> Text
  -> Value
  -> IO (Either ConnectorError Value)
doGraphQL mgr apiKey query vars = do
  initReq <- parseRequest "POST https://api.linear.app/graphql"
  let body    = encode (object ["query" .= query, "variables" .= vars])
      -- Normalise the stored key: strip whitespace and any accidental
      -- "Bearer " prefix that users sometimes include when copying from docs.
      rawKeyText  = T.strip (decodeUtf8With lenientDecode apiKey)
      cleanKey    = maybe rawKeyText id (T.stripPrefix "Bearer " rawKeyText)
      trimmedKey  = encodeUtf8 cleanKey
      authHdr     = "Bearer " <> trimmedKey
      req     = initReq
        { method         = "POST"
        , requestBody    = RequestBodyLBS body
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", authHdr)
            ]
        }
  resp <- httpLbs req mgr
  let status = statusCode (responseStatus resp)
      body'  = responseBody resp
  case status of
    401 -> pure $ Left $ ConnectorInvalidCredential "Linear API key is invalid or expired (401)"
    403 -> pure $ Left $ ConnectorInvalidCredential "Linear API key lacks required permissions (403)"
    429 -> pure $ Left $ ConnectorApiError "Linear API rate limit exceeded (429)"
    200 -> parseGraphQLResponse body'
    _   -> pure $ Left $ ConnectorApiError $
             "Linear API returned HTTP " <> T.pack (show status) <> ": "
             <> decodeUtf8With lenientDecode (BL.toStrict (BL.take 500 body'))

-- | Parse a Linear GraphQL response body.
-- Returns the @data@ field on success, or maps @errors@ to a 'ConnectorError'.
parseGraphQLResponse :: BL.ByteString -> IO (Either ConnectorError Value)
parseGraphQLResponse body =
  case decode body of
    Nothing ->
      pure $ Left $ ConnectorApiError $
        "failed to parse Linear response as JSON: "
        <> T.pack (show (BL.take 200 body))
    Just v ->
      case parseMaybe extractErrors v of
        Just errMsg -> pure $ Left $ ConnectorApiError errMsg
        Nothing ->
          case parseMaybe extractData v of
            Just dataVal -> pure (Right dataVal)
            Nothing      -> pure $ Left $ ConnectorApiError "Linear response missing 'data' field"

extractErrors :: Value -> Parser Text
extractErrors = withObject "response" $ \o -> do
  errs <- o .: "errors"
  case errs of
    Array arr ->
      case listToMaybe (foldr (:) [] arr) of
        Nothing   -> fail "no errors"
        Just first ->
          case parseMaybe (withObject "error" (.: "message")) first of
            Just msg -> pure msg
            Nothing  -> pure "unknown GraphQL error"
    _ -> fail "no errors"

extractData :: Value -> Parser Value
extractData = withObject "response" (.: "data")

-- ---------------------------------------------------------------------------
-- Utility
-- ---------------------------------------------------------------------------

-- | Look up a 'Text' value from a JSON object by key.
lookupText :: Text -> Value -> Maybe Text
lookupText key v = parseMaybe (withObject "obj" (.: fromText key)) v
