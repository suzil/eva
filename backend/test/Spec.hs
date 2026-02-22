module Main where

import Test.Hspec

import qualified Eva.Api.ServerSpec
import qualified Eva.Core.TypesSpec
import qualified Eva.Persistence.PersistenceSpec

main :: IO ()
main = hspec $ do
  Eva.Api.ServerSpec.spec
  Eva.Core.TypesSpec.spec
  Eva.Persistence.PersistenceSpec.spec
