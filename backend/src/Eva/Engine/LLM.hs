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
  , dummyLLMClient

    -- * Request / Response types
  , LLMRequest (..)
  , ChatMessage (..)
  , LLMResponse (..)
  , TokenUsage (..)
  , LLMError (..)

    -- * Exposed for testing
  , parseSseLine
  , classifyStatus
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Aeson (Value (..), decode, encode, object, (.:), (.=))
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
-- Request / Response types
-- ---------------------------------------------------------------------------

data ChatMessage = ChatMessage
  { chatRole    :: Text
  , chatContent :: Text
  }
  deriving (Eq, Show)

data LLMRequest = LLMRequest
  { llmModel          :: Text
  , llmMessages       :: [ChatMessage]
  , llmTemperature    :: Double
  , llmMaxTokens      :: Maybe Int
  , llmResponseFormat :: ResponseFormat
  }
  deriving (Eq, Show)

data TokenUsage = TokenUsage
  { usagePromptTokens     :: Int
  , usageCompletionTokens :: Int
  , usageTotalTokens      :: Int
  }
  deriving (Eq, Show)

data LLMResponse = LLMResponse
  { llmContent :: Text
  , llmUsage   :: TokenUsage
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
-- Every call immediately returns 'Left (LLMAuthError "no LLM client configured")'.
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
        pure (Right (LLMResponse content usage))

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
      ++ [ "stream_options" .= object ["include_usage" .= True] | streaming ]

messageToJson :: ChatMessage -> Value
messageToJson m = object ["role" .= chatRole m, "content" .= chatContent m]

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
  content <- case choices of
    (first : _) -> first .: "message" >>= (.: "content")
    []          -> fail "empty choices array"
  usage         <- o .: "usage"
  promptTok     <- usage .: "prompt_tokens"
  completionTok <- usage .: "completion_tokens"
  totalTok      <- usage .: "total_tokens"
  pure LLMResponse
    { llmContent = content
    , llmUsage   = TokenUsage
        { usagePromptTokens     = promptTok
        , usageCompletionTokens = completionTok
        , usageTotalTokens      = totalTok
        }
    }

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
