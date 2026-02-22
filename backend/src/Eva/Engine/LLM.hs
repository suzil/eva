{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | LLM provider abstraction: OpenAI (EVA-23), Anthropic (EVA-42).
-- Streaming via http-client streaming body reader.
--
-- 'LLMClient' is a record-of-functions so providers are injectable
-- without type-parameter complexity. Tests use 'dummyLLMClient';
-- production callers use 'mkOpenAIClient'.
module Eva.Engine.LLM
  ( -- * Client
    LLMClient (..)
  , mkOpenAIClient
  , mkAnthropicClient
  , dummyLLMClient

    -- * Request / Response types
  , LLMRequest (..)
  , ChatMessage (..)
  , LLMResponse (..)
  , TokenUsage (..)
  , LLMError (..)

    -- * Tool-calling types
  , ToolSpec (..)
  , ToolCall (..)

    -- * Exposed for testing
  , parseSseLine
  , classifyStatus
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Aeson (Value (..), decode, encode, object, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (..)
  , Response (..)
  , httpLbs
  , parseRequest
  , withResponse
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (statusCode)

import Eva.Core.Types (ResponseFormat (..))

-- ---------------------------------------------------------------------------
-- Error type
-- ---------------------------------------------------------------------------

data LLMError
  = LLMAuthError Text
  | LLMRateLimitError Text
  | LLMApiError Int Text
  | LLMTimeoutError
  | LLMParseError Text
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Tool-calling types
-- ---------------------------------------------------------------------------

-- | An LLM tool derived from a connector's 'ActionSpec'.
data ToolSpec = ToolSpec
  { toolName        :: Text
  , toolDescription :: Text
  , toolParameters  :: Value  -- ^ JSON Schema object
  }
  deriving (Eq, Show)

-- | A tool call returned by the LLM (finish_reason == "tool_calls").
data ToolCall = ToolCall
  { toolCallId   :: Text
  , toolCallName :: Text
  , toolCallArgs :: Value  -- ^ Parsed JSON arguments
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Request / Response types
-- ---------------------------------------------------------------------------

-- | A chat conversation turn.
--
-- 'ChatMessage' covers system/user/assistant text messages (existing usage).
-- 'ToolCallMsg' is the assistant turn that contains tool calls instead of text.
-- 'ToolResultMsg' is the tool role message feeding results back to the LLM.
data ChatMessage
  = ChatMessage { chatRole :: Text, chatContent :: Text }
  | ToolCallMsg  [ToolCall]
  | ToolResultMsg Text Text  -- ^ tool_call_id, result content
  deriving (Eq, Show)

data LLMRequest = LLMRequest
  { llmModel          :: Text
  , llmMessages       :: [ChatMessage]
  , llmTemperature    :: Double
  , llmMaxTokens      :: Maybe Int
  , llmResponseFormat :: ResponseFormat
  , llmTools          :: [ToolSpec]  -- ^ Empty list = no tool calling
  }
  deriving (Eq, Show)

data TokenUsage = TokenUsage
  { usagePromptTokens     :: Int
  , usageCompletionTokens :: Int
  , usageTotalTokens      :: Int
  }
  deriving (Eq, Show)

data LLMResponse = LLMResponse
  { llmContent   :: Text             -- ^ Empty when tool calls present
  , llmToolCalls :: Maybe [ToolCall] -- ^ 'Just' when finish_reason == "tool_calls"
  , llmUsage     :: TokenUsage
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Provider abstraction
-- ---------------------------------------------------------------------------

-- | Injectable LLM provider. Production: 'mkOpenAIClient'. Tests: 'dummyLLMClient'.
data LLMClient = LLMClient
  { clientCall   :: LLMRequest -> IO (Either LLMError LLMResponse)
    -- ^ Blocking call — accumulates the full response before returning.
  , clientStream :: LLMRequest -> (Text -> IO ()) -> IO (Either LLMError LLMResponse)
    -- ^ Streaming call — fires the token callback for each delta, then returns
    -- the complete response. The callback receives individual token strings.
  }

-- | A no-op client for use in tests and when no API key is configured.
dummyLLMClient :: LLMClient
dummyLLMClient = LLMClient
  { clientCall   = \_ -> pure (Left (LLMAuthError "no LLM client configured"))
  , clientStream = \_ _ -> pure (Left (LLMAuthError "no LLM client configured"))
  }

-- | Build an OpenAI-backed 'LLMClient' from an API key. Creates a fresh TLS
-- manager; prefer sharing a single manager across requests in production.
mkOpenAIClient :: Text -> IO LLMClient
mkOpenAIClient apiKey = do
  mgr <- newTlsManager
  pure LLMClient
    { clientCall   = openAICall   apiKey mgr
    , clientStream = openAIStream apiKey mgr
    }

-- ---------------------------------------------------------------------------
-- OpenAI — non-streaming
-- ---------------------------------------------------------------------------

openAICall :: Text -> Manager -> LLMRequest -> IO (Either LLMError LLMResponse)
openAICall apiKey mgr req = do
  result <- try (doCall apiKey mgr req)
  case result of
    Left (e :: SomeException) -> pure (Left (LLMApiError 0 (T.pack (show e))))
    Right v                   -> pure v

doCall :: Text -> Manager -> LLMRequest -> IO (Either LLMError LLMResponse)
doCall apiKey mgr req = do
  httpReq  <- buildRequest apiKey req False
  response <- httpLbs httpReq mgr
  let status = statusCode (responseStatus response)
      body   = responseBody response
  case classifyStatus status body of
    Left err -> pure (Left err)
    Right () -> parseNonStreamingBody body

-- ---------------------------------------------------------------------------
-- OpenAI — streaming (SSE)
-- ---------------------------------------------------------------------------

openAIStream
  :: Text
  -> Manager
  -> LLMRequest
  -> (Text -> IO ())
  -> IO (Either LLMError LLMResponse)
openAIStream apiKey mgr req onToken = do
  result <- try (doStream apiKey mgr req onToken)
  case result of
    Left (e :: SomeException) -> pure (Left (LLMApiError 0 (T.pack (show e))))
    Right v                   -> pure v

doStream
  :: Text
  -> Manager
  -> LLMRequest
  -> (Text -> IO ())
  -> IO (Either LLMError LLMResponse)
doStream apiKey mgr req onToken = do
  httpReq <- buildRequest apiKey req True
  withResponse httpReq mgr $ \response -> do
    let status     = statusCode (responseStatus response)
        bodyReader = responseBody response  -- :: IO ByteString
    if status /= 200
      then do
        -- Non-200: not SSE, read the full error body.
        errBody <- BL.fromStrict <$> readAll bodyReader
        case classifyStatus status errBody of
          Left err -> pure (Left err)
          Right () -> pure (Left (LLMApiError status "unexpected non-200 response"))
      else do
        tokensRef  <- newIORef ([] :: [Text])
        usageRef   <- newIORef (Nothing :: Maybe TokenUsage)
        leftoverRef <- newIORef BS.empty
        let loop = do
              chunk <- bodyReader
              unless (BS.null chunk) $ do
                prev <- readIORef leftoverRef
                let combined         = prev <> chunk
                    (ls, remainder) = splitOnNewlines combined
                writeIORef leftoverRef remainder
                mapM_ (processLine tokensRef usageRef onToken) ls
                loop
        loop
        -- Flush any unterminated final line.
        remaining <- readIORef leftoverRef
        unless (BS.null remaining) $
          processLine tokensRef usageRef onToken remaining
        tokens <- readIORef tokensRef
        mUsage <- readIORef usageRef
        let content = T.concat (reverse tokens)
            usage   = case mUsage of
              Just u  -> u
              Nothing -> TokenUsage 0 0 0
        pure (Right (LLMResponse content Nothing usage))

-- | Decode and process one SSE line, updating the accumulators.
processLine
  :: IORef [Text]
  -> IORef (Maybe TokenUsage)
  -> (Text -> IO ())
  -> ByteString
  -> IO ()
processLine tokensRef usageRef onToken raw =
  case parseSseLine (TE.decodeUtf8Lenient raw) of
    Nothing  -> pure ()
    Just val ->
      case parseMaybe parseStreamChunk val of
        Nothing            -> pure ()
        Just (mTok, mUsage) -> do
          case mTok of
            Just t  -> do modifyIORef' tokensRef (t :); onToken t
            Nothing -> pure ()
          case mUsage of
            Just u  -> writeIORef usageRef (Just u)
            Nothing -> pure ()

-- ---------------------------------------------------------------------------
-- SSE / JSON parsing helpers (exported for testing)
-- ---------------------------------------------------------------------------

-- | Parse a single SSE data line into a JSON 'Value'.
-- Returns 'Nothing' for @[DONE]@, blank lines, comment lines, and parse failures.
parseSseLine :: Text -> Maybe Value
parseSseLine line
  | T.null (T.strip line)              = Nothing
  | not ("data:" `T.isPrefixOf` line) = Nothing
  | otherwise =
      let payload = T.strip (T.drop 5 line)
      in if payload == "[DONE]"
           then Nothing
           else decode (BL.fromStrict (TE.encodeUtf8 payload))

-- | Classify an HTTP status code into a structured 'LLMError' or 'Right ()'.
classifyStatus :: Int -> BL.ByteString -> Either LLMError ()
classifyStatus 200 _    = Right ()
classifyStatus 401 body = Left (LLMAuthError     (extractMessage body))
classifyStatus 403 body = Left (LLMAuthError     (extractMessage body))
classifyStatus 429 body = Left (LLMRateLimitError (extractMessage body))
classifyStatus n   body = Left (LLMApiError n    (extractMessage body))

-- | Pull the @error.message@ string out of an OpenAI error body, falling back
-- to a raw text excerpt on parse failure.
extractMessage :: BL.ByteString -> Text
extractMessage body =
  case decode body of
    Just (Object o) ->
      case parseMaybe (\obj -> (obj .: "error") >>= (.: "message")) o of
        Just msg -> msg
        Nothing  -> excerpt
    _ -> excerpt
  where
    excerpt = T.take 200 (TE.decodeUtf8Lenient (BL.toStrict body))

-- ---------------------------------------------------------------------------
-- JSON building / parsing
-- ---------------------------------------------------------------------------

-- | Serialize an 'LLMRequest' to the OpenAI chat completions request body.
buildRequestBody :: LLMRequest -> Bool -> BL.ByteString
buildRequestBody req streaming =
  encode $
    object $
      [ "model"       .= llmModel req
      , "messages"    .= map messageToJson (llmMessages req)
      , "temperature" .= llmTemperature req
      , "stream"      .= streaming
      ]
      ++ maybe [] (\n -> ["max_tokens" .= n]) (llmMaxTokens req)
      ++ formatField (llmResponseFormat req)
      ++ toolsField (llmTools req)
      ++ [ "stream_options" .= object ["include_usage" .= True] | streaming ]

toolsField :: [ToolSpec] -> [Pair]
toolsField [] = []
toolsField ts =
  [ "tools" .= map toolSpecToJson ts
  , "tool_choice" .= ("auto" :: Text)
  ]

toolSpecToJson :: ToolSpec -> Value
toolSpecToJson t = object
  [ "type"     .= ("function" :: Text)
  , "function" .= object
      [ "name"        .= toolName t
      , "description" .= toolDescription t
      , "parameters"  .= toolParameters t
      ]
  ]

messageToJson :: ChatMessage -> Value
messageToJson (ChatMessage role content) =
  object ["role" .= role, "content" .= content]
messageToJson (ToolCallMsg calls) =
  object
    [ "role"       .= ("assistant" :: Text)
    , "content"    .= Null
    , "tool_calls" .= map toolCallToJson calls
    ]
messageToJson (ToolResultMsg callId content) =
  object
    [ "role"         .= ("tool" :: Text)
    , "tool_call_id" .= callId
    , "content"      .= content
    ]

toolCallToJson :: ToolCall -> Value
toolCallToJson tc = object
  [ "id"   .= toolCallId tc
  , "type" .= ("function" :: Text)
  , "function" .= object
      [ "name"      .= toolCallName tc
        -- OpenAI format: arguments is a JSON-encoded string, not an object
      , "arguments" .= TE.decodeUtf8Lenient (BL.toStrict (encode (toolCallArgs tc)))
      ]
  ]

formatField :: ResponseFormat -> [Pair]
formatField ResponseText = []
formatField ResponseJson = [("response_format", object ["type" .= ("json_object" :: Text)])]

-- | Build an http-client 'Request' for the OpenAI chat completions endpoint.
buildRequest :: Text -> LLMRequest -> Bool -> IO Request
buildRequest apiKey req streaming = do
  baseReq <- parseRequest "POST https://api.openai.com/v1/chat/completions"
  pure baseReq
    { requestHeaders =
        [ ("Content-Type",  "application/json")
        , ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey)
        ]
    , requestBody = RequestBodyLBS (buildRequestBody req streaming)
    }

-- | Parse a complete (non-streaming) OpenAI response body.
parseNonStreamingBody :: BL.ByteString -> IO (Either LLMError LLMResponse)
parseNonStreamingBody body =
  case decode body of
    Nothing  -> pure (Left (LLMParseError "failed to decode JSON response"))
    Just val -> case parseMaybe parseCompletion val of
      Nothing   -> pure (Left (LLMParseError "unexpected response structure"))
      Just resp -> pure (Right resp)

parseCompletion :: Value -> Parser LLMResponse
parseCompletion = Aeson.withObject "Completion" $ \o -> do
  choices <- o .: "choices"
  (content, mToolCalls) <- case choices of
    (first : _) -> do
      finishReason <- first .:? "finish_reason" :: Parser (Maybe Text)
      msg          <- first .: "message"
      if finishReason == Just "tool_calls"
        then do
          rawCalls <- msg .: "tool_calls"
          calls    <- mapM parseToolCall rawCalls
          pure ("", Just calls)
        else do
          txt <- msg .: "content"
          pure (txt, Nothing)
    [] -> fail "empty choices array"
  usage         <- o .: "usage"
  promptTok     <- usage .: "prompt_tokens"
  completionTok <- usage .: "completion_tokens"
  totalTok      <- usage .: "total_tokens"
  pure LLMResponse
    { llmContent   = content
    , llmToolCalls = mToolCalls
    , llmUsage     = TokenUsage
        { usagePromptTokens     = promptTok
        , usageCompletionTokens = completionTok
        , usageTotalTokens      = totalTok
        }
    }

parseToolCall :: Value -> Parser ToolCall
parseToolCall = Aeson.withObject "ToolCall" $ \o -> do
  callId   <- o .: "id"
  fn       <- o .: "function"
  name     <- fn .: "name"
  argsText <- fn .: "arguments"  -- JSON-encoded string
  let args = case Aeson.decode (BL.fromStrict (TE.encodeUtf8 argsText)) of
               Just v  -> v
               Nothing -> Aeson.String argsText  -- fallback: treat as raw text
  pure ToolCall { toolCallId = callId, toolCallName = name, toolCallArgs = args }

-- | Parse a streaming chunk: returns optional token delta and optional usage
-- (the final chunk carries usage when @stream_options.include_usage@ is set).
parseStreamChunk :: Value -> Parser (Maybe Text, Maybe TokenUsage)
parseStreamChunk = Aeson.withObject "StreamChunk" $ \o -> do
  choices <- o .: "choices"
  mTok <- case choices of
    (first : _) -> do
      delta <- first .: "delta"
      delta Aeson..:? "content"
    [] -> pure Nothing
  mUsageObj <- o Aeson..:? "usage"
  mUsage <- case mUsageObj of
    Nothing -> pure Nothing
    Just u  -> do
      promptTok     <- u .: "prompt_tokens"
      completionTok <- u .: "completion_tokens"
      totalTok      <- u .: "total_tokens"
      pure $ Just TokenUsage
        { usagePromptTokens     = promptTok
        , usageCompletionTokens = completionTok
        , usageTotalTokens      = totalTok
        }
  pure (mTok, mUsage)

-- ---------------------------------------------------------------------------
-- Anthropic — client constructor
-- ---------------------------------------------------------------------------

-- | Build an Anthropic-backed 'LLMClient' from an API key.
mkAnthropicClient :: Text -> IO LLMClient
mkAnthropicClient apiKey = do
  mgr <- newTlsManager
  pure LLMClient
    { clientCall   = anthropicCall   apiKey mgr
    , clientStream = anthropicStream apiKey mgr
    }

-- ---------------------------------------------------------------------------
-- Anthropic — non-streaming
-- ---------------------------------------------------------------------------

anthropicCall :: Text -> Manager -> LLMRequest -> IO (Either LLMError LLMResponse)
anthropicCall apiKey mgr req = do
  result <- try (doAnthropicCall apiKey mgr req)
  case result of
    Left (e :: SomeException) -> pure (Left (LLMApiError 0 (T.pack (show e))))
    Right v                   -> pure v

doAnthropicCall :: Text -> Manager -> LLMRequest -> IO (Either LLMError LLMResponse)
doAnthropicCall apiKey mgr req = do
  httpReq  <- buildAnthropicRequest apiKey req False
  response <- httpLbs httpReq mgr
  let status = statusCode (responseStatus response)
      body   = responseBody response
  case classifyAnthropicStatus status body of
    Left err -> pure (Left err)
    Right () -> parseAnthropicBody body

-- ---------------------------------------------------------------------------
-- Anthropic — streaming (SSE)
-- ---------------------------------------------------------------------------

anthropicStream
  :: Text
  -> Manager
  -> LLMRequest
  -> (Text -> IO ())
  -> IO (Either LLMError LLMResponse)
anthropicStream apiKey mgr req onToken = do
  result <- try (doAnthropicStream apiKey mgr req onToken)
  case result of
    Left (e :: SomeException) -> pure (Left (LLMApiError 0 (T.pack (show e))))
    Right v                   -> pure v

doAnthropicStream
  :: Text
  -> Manager
  -> LLMRequest
  -> (Text -> IO ())
  -> IO (Either LLMError LLMResponse)
doAnthropicStream apiKey mgr req onToken = do
  httpReq <- buildAnthropicRequest apiKey req True
  withResponse httpReq mgr $ \response -> do
    let status     = statusCode (responseStatus response)
        bodyReader = responseBody response
    if status /= 200
      then do
        errBody <- BL.fromStrict <$> readAll bodyReader
        case classifyAnthropicStatus status errBody of
          Left err -> pure (Left err)
          Right () -> pure (Left (LLMApiError status "unexpected non-200 response"))
      else do
        tokensRef    <- newIORef ([] :: [Text])
        inputTokRef  <- newIORef (0 :: Int)
        outputTokRef <- newIORef (0 :: Int)
        leftoverRef  <- newIORef BS.empty
        let loop = do
              chunk <- bodyReader
              unless (BS.null chunk) $ do
                prev <- readIORef leftoverRef
                let combined         = prev <> chunk
                    (ls, remainder) = splitOnNewlines combined
                writeIORef leftoverRef remainder
                mapM_ (processAnthropicLine tokensRef inputTokRef outputTokRef onToken) ls
                loop
        loop
        remaining <- readIORef leftoverRef
        unless (BS.null remaining) $
          processAnthropicLine tokensRef inputTokRef outputTokRef onToken remaining
        tokens    <- readIORef tokensRef
        inputTok  <- readIORef inputTokRef
        outputTok <- readIORef outputTokRef
        let content = T.concat (reverse tokens)
            usage   = TokenUsage inputTok outputTok (inputTok + outputTok)
        pure (Right (LLMResponse content Nothing usage))

processAnthropicLine
  :: IORef [Text]
  -> IORef Int
  -> IORef Int
  -> (Text -> IO ())
  -> ByteString
  -> IO ()
processAnthropicLine tokensRef inputTokRef outputTokRef onToken raw =
  case parseSseLine (TE.decodeUtf8Lenient raw) of
    Nothing  -> pure ()
    Just val ->
      case parseMaybe parseAnthropicStreamChunk val of
        Nothing                            -> pure ()
        Just (mTok, mInputTok, mOutputTok) -> do
          case mTok of
            Just t  -> do modifyIORef' tokensRef (t :); onToken t
            Nothing -> pure ()
          case mInputTok of
            Just n  -> writeIORef inputTokRef n
            Nothing -> pure ()
          case mOutputTok of
            Just n  -> writeIORef outputTokRef n
            Nothing -> pure ()

-- | Parse an Anthropic SSE data chunk.
-- Returns (optional text delta, optional input_tokens, optional output_tokens).
parseAnthropicStreamChunk :: Value -> Parser (Maybe Text, Maybe Int, Maybe Int)
parseAnthropicStreamChunk = Aeson.withObject "AnthropicChunk" $ \o -> do
  eventType <- o .: "type" :: Parser Text
  case eventType of
    "content_block_delta" -> do
      delta     <- o .: "delta"
      deltaType <- delta .: "type" :: Parser Text
      mTok <- if deltaType == "text_delta"
                then Just <$> (delta .: "text")
                else pure Nothing
      pure (mTok, Nothing, Nothing)
    "message_start" -> do
      msg   <- o .: "message"
      usage <- msg .: "usage"
      inputTok <- usage .: "input_tokens"
      pure (Nothing, Just inputTok, Nothing)
    "message_delta" -> do
      usage     <- o .: "usage"
      outputTok <- usage .: "output_tokens"
      pure (Nothing, Nothing, Just outputTok)
    _ -> pure (Nothing, Nothing, Nothing)

-- ---------------------------------------------------------------------------
-- Anthropic — request building
-- ---------------------------------------------------------------------------

-- | Build an http-client 'Request' for the Anthropic messages endpoint.
buildAnthropicRequest :: Text -> LLMRequest -> Bool -> IO Request
buildAnthropicRequest apiKey req streaming = do
  baseReq <- parseRequest "POST https://api.anthropic.com/v1/messages"
  pure baseReq
    { requestHeaders =
        [ ("Content-Type",      "application/json")
        , ("x-api-key",         TE.encodeUtf8 apiKey)
        , ("anthropic-version", "2023-06-01")
        ]
    , requestBody = RequestBodyLBS (buildAnthropicRequestBody req streaming)
    }

-- | Serialize an 'LLMRequest' to the Anthropic messages request body.
-- Extracts the system message (role=="system") and places it at top level;
-- the remaining messages are serialized in Anthropic format.
-- Groups consecutive 'ToolResultMsg' entries into a single user message.
buildAnthropicRequestBody :: LLMRequest -> Bool -> BL.ByteString
buildAnthropicRequestBody req streaming =
  let allMsgs     = llmMessages req
      systemText  = extractSystemText allMsgs
      otherMsgs   = filter (not . isSystemMsg) allMsgs
      msgValues   = buildAnthropicMessages otherMsgs
      maxTok      = maybe 4096 id (llmMaxTokens req)
  in encode $
       object $
         [ "model"       .= llmModel req
         , "max_tokens"  .= maxTok
         , "messages"    .= msgValues
         , "stream"      .= streaming
         ]
         ++ maybe [] (\s -> ["system" .= s]) systemText
         ++ anthropicToolsField (llmTools req)

isSystemMsg :: ChatMessage -> Bool
isSystemMsg (ChatMessage role _) = role == "system"
isSystemMsg _                    = False

extractSystemText :: [ChatMessage] -> Maybe Text
extractSystemText msgs =
  case [c | ChatMessage "system" c <- msgs] of
    []    -> Nothing
    (t:_) -> Just t

-- | Convert a list of 'ChatMessage' to Anthropic message JSON, combining
-- consecutive 'ToolResultMsg' entries into a single user message.
buildAnthropicMessages :: [ChatMessage] -> [Value]
buildAnthropicMessages = go
  where
    go [] = []
    go (ToolResultMsg callId content : rest) =
      let (moreResults, after) = span isToolResult rest
          allResults = ToolResultMsg callId content : moreResults
          block = object
            [ "role"    .= ("user" :: Text)
            , "content" .= map toolResultToContent allResults
            ]
      in block : go after
    go (msg : rest) = anthropicMessageToJson msg : go rest

    isToolResult (ToolResultMsg _ _) = True
    isToolResult _                   = False

    toolResultToContent (ToolResultMsg callId content) =
      object
        [ "type"        .= ("tool_result" :: Text)
        , "tool_use_id" .= callId
        , "content"     .= content
        ]
    toolResultToContent _ = Null

anthropicMessageToJson :: ChatMessage -> Value
anthropicMessageToJson (ChatMessage role content) =
  object ["role" .= role, "content" .= content]
anthropicMessageToJson (ToolCallMsg calls) =
  object
    [ "role"    .= ("assistant" :: Text)
    , "content" .= map anthropicToolUseBlock calls
    ]
anthropicMessageToJson (ToolResultMsg callId content) =
  object
    [ "role"    .= ("user" :: Text)
    , "content" .=
        [ object
            [ "type"        .= ("tool_result" :: Text)
            , "tool_use_id" .= callId
            , "content"     .= content
            ]
        ]
    ]

anthropicToolUseBlock :: ToolCall -> Value
anthropicToolUseBlock tc = object
  [ "type"  .= ("tool_use" :: Text)
  , "id"    .= toolCallId tc
  , "name"  .= toolCallName tc
  , "input" .= toolCallArgs tc
  ]

anthropicToolsField :: [ToolSpec] -> [Pair]
anthropicToolsField [] = []
anthropicToolsField ts = ["tools" .= map anthropicToolSpecToJson ts]

anthropicToolSpecToJson :: ToolSpec -> Value
anthropicToolSpecToJson t = object
  [ "name"         .= toolName t
  , "description"  .= toolDescription t
  , "input_schema" .= toolParameters t
  ]

-- ---------------------------------------------------------------------------
-- Anthropic — response parsing
-- ---------------------------------------------------------------------------

-- | Parse a complete (non-streaming) Anthropic messages response.
parseAnthropicBody :: BL.ByteString -> IO (Either LLMError LLMResponse)
parseAnthropicBody body =
  case decode body of
    Nothing  -> pure (Left (LLMParseError "failed to decode Anthropic JSON response"))
    Just val -> case parseMaybe parseAnthropicCompletion val of
      Nothing   -> pure (Left (LLMParseError "unexpected Anthropic response structure"))
      Just resp -> pure (Right resp)

parseAnthropicCompletion :: Value -> Parser LLMResponse
parseAnthropicCompletion = Aeson.withObject "AnthropicMessage" $ \o -> do
  contentBlocks <- o .: "content" :: Parser [Value]
  stopReason    <- o .:? "stop_reason" :: Parser (Maybe Text)
  usage         <- o .: "usage"
  inputTok      <- usage .: "input_tokens"
  outputTok     <- usage .: "output_tokens"
  let tokenUsage = TokenUsage
        { usagePromptTokens     = inputTok
        , usageCompletionTokens = outputTok
        , usageTotalTokens      = inputTok + outputTok
        }
  if stopReason == Just "tool_use"
    then do
      calls <- mapM parseAnthropicToolUse
                 [ b | b@(Object bo) <- contentBlocks
                     , parseMaybe (.: "type") bo == Just ("tool_use" :: Text)
                 ]
      pure LLMResponse
        { llmContent   = ""
        , llmToolCalls = if null calls then Nothing else Just calls
        , llmUsage     = tokenUsage
        }
    else do
      let textContent = T.concat
            [ t
            | block <- contentBlocks
            , Just t <- [parseMaybe extractBlockText block]
            ]
      pure LLMResponse
        { llmContent   = textContent
        , llmToolCalls = Nothing
        , llmUsage     = tokenUsage
        }

extractBlockText :: Value -> Parser Text
extractBlockText = Aeson.withObject "ContentBlock" $ \o -> do
  blockType <- o .: "type" :: Parser Text
  case blockType of
    "text" -> o .: "text"
    _      -> fail "not a text block"

parseAnthropicToolUse :: Value -> Parser ToolCall
parseAnthropicToolUse = Aeson.withObject "ToolUseBlock" $ \o -> do
  callId <- o .: "id"
  name   <- o .: "name"
  input  <- o .: "input"  -- already a JSON object, no decoding needed
  pure ToolCall { toolCallId = callId, toolCallName = name, toolCallArgs = input }

-- ---------------------------------------------------------------------------
-- Anthropic — status classification
-- ---------------------------------------------------------------------------

classifyAnthropicStatus :: Int -> BL.ByteString -> Either LLMError ()
classifyAnthropicStatus 200 _    = Right ()
classifyAnthropicStatus 401 body = Left (LLMAuthError     (extractAnthropicMessage body))
classifyAnthropicStatus 403 body = Left (LLMAuthError     (extractAnthropicMessage body))
classifyAnthropicStatus 429 body = Left (LLMRateLimitError (extractAnthropicMessage body))
classifyAnthropicStatus n   body = Left (LLMApiError n    (extractAnthropicMessage body))

-- | Pull the @error.message@ string from an Anthropic error body.
extractAnthropicMessage :: BL.ByteString -> Text
extractAnthropicMessage body =
  case decode body of
    Just (Object o) ->
      case parseMaybe (\obj -> (obj .: "error") >>= (.: "message")) o of
        Just msg -> msg
        Nothing  -> excerpt
    _ -> excerpt
  where
    excerpt = T.take 200 (TE.decodeUtf8Lenient (BL.toStrict body))

-- ---------------------------------------------------------------------------
-- ByteString utilities
-- ---------------------------------------------------------------------------

-- | Split a 'ByteString' on @\\n@ boundaries.
-- Returns (complete lines, unterminated remainder).
splitOnNewlines :: ByteString -> ([ByteString], ByteString)
splitOnNewlines bs =
  let parts = BS.split 10 bs  -- 10 == '\n'
  in case parts of
       []  -> ([], BS.empty)
       _   -> (init parts, last parts)

-- | Drain a streaming body reader into a single strict 'ByteString'.
readAll :: IO ByteString -> IO ByteString
readAll reader = go []
  where
    go acc = do
      chunk <- reader
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (chunk : acc)
