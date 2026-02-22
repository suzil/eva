module Main where

import Test.Hspec

import qualified Eva.Api.ServerSpec
import qualified Eva.Api.WebSocketSpec
import qualified Eva.Core.GraphSpec
import qualified Eva.Core.TypesSpec
import qualified Eva.Core.ValidationSpec
import qualified Eva.Engine.Handlers.ActionSpec
import qualified Eva.Engine.Handlers.AgentSpec
import qualified Eva.Engine.Handlers.ConnectorSpec
import qualified Eva.Engine.Handlers.TriggerSpec
import qualified Eva.Engine.LLMSpec
import qualified Eva.Engine.RetrySpec
import qualified Eva.Engine.RunnerSpec
import qualified Eva.Engine.StateMachineSpec
import qualified Eva.Persistence.PersistenceSpec

main :: IO ()
main = hspec $ do
  Eva.Api.ServerSpec.spec
  Eva.Api.WebSocketSpec.spec
  Eva.Core.GraphSpec.spec
  Eva.Core.TypesSpec.spec
  Eva.Core.ValidationSpec.spec
  Eva.Engine.Handlers.ActionSpec.spec
  Eva.Engine.Handlers.AgentSpec.spec
  Eva.Engine.Handlers.ConnectorSpec.spec
  Eva.Engine.Handlers.TriggerSpec.spec
  Eva.Engine.LLMSpec.spec
  Eva.Engine.RetrySpec.spec
  Eva.Engine.RunnerSpec.spec
  Eva.Engine.StateMachineSpec.spec
  Eva.Persistence.PersistenceSpec.spec
