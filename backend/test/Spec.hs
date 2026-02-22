module Main where

import Test.Hspec

import qualified Eva.Core.TypesSpec

main :: IO ()
main = hspec $ do
  Eva.Core.TypesSpec.spec
