{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- Health check API: GET /api/health
type HealthAPI = "api" :> "health" :> Get '[JSON] HealthResponse

newtype HealthResponse = HealthResponse {status :: Text}
  deriving (Show)

instance ToJSON HealthResponse where
  toJSON (HealthResponse s) = object ["status" .= s]

healthHandler :: Handler HealthResponse
healthHandler = pure (HealthResponse "ok")

app :: Application
app = serve (Proxy :: Proxy HealthAPI) healthHandler

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  let port = maybe 8080 id (portEnv >>= readMaybe)
  putStrLn $ "Eva backend listening on port " <> show port
  run port app
