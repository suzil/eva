{-# LANGUAGE OverloadedStrings #-}

module Main where

import Eva.Api.Server (makeApp)
import Eva.App (logMsg, makeAppEnv, runAppM)
import Eva.Config (LogLevel (..), configPort, loadConfig)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  cfg <- loadConfig
  env <- makeAppEnv cfg
  runAppM env $ logMsg LogInfo "Eva backend starting"
  let port = configPort cfg
  run port (makeApp env)
