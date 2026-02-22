{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Eva.App (logMsg, makeAppEnv, runAppM)
import Eva.Config (LogLevel (..), configPort, loadConfig)
import Network.Wai.Handler.Warp (run)
import Servant

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
  cfg <- loadConfig
  env <- makeAppEnv cfg
  runAppM env $ logMsg LogInfo "Eva backend starting"
  let port = configPort cfg
  run port app
