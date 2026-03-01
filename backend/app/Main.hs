{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit (exitSuccess)
import System.Posix.Signals (Handler (..), installHandler, sigTERM)

import Eva.Api.Server (makeApp)
import Eva.App (AppEnv (..), logMsg, makeAppEnv, runAppM)
import Eva.Config (LogLevel (..), configPort, loadConfig)
import Eva.Engine.Dispatch (execute)
import Eva.Engine.Scheduler (shutdownScheduler, startScheduler)
import Eva.Prompt.Store (seedBuiltinTemplates)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort, setTimeout)

main :: IO ()
main = do
  cfg <- loadConfig
  env <- makeAppEnv cfg execute
  seedBuiltinTemplates (envDbPool env)
  runAppM env $ logMsg LogInfo "Eva backend starting"

  -- Start the in-process cron scheduler.
  schedulerHandle <- startScheduler env

  -- Graceful SIGTERM: stop the scheduler (waits for in-flight runs up to 30 s)
  -- then exit. The Warp server is not explicitly stopped; the process exits.
  _ <- installHandler sigTERM
        (Catch $ do
            runAppM env $ logMsg LogInfo "Received SIGTERM — shutting down"
            shutdownScheduler schedulerHandle
            runAppM env $ logMsg LogInfo "Shutdown complete"
            exitSuccess
        ) Nothing

  let port = configPort cfg
      warpSettings = setTimeout 300 $ setPort port defaultSettings
  runSettings warpSettings (makeApp env)
