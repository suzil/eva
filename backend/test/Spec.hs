module Main where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Eva" $ do
    it "placeholder — tests added per ticket" $
      True `shouldBe` True
    it "QuickCheck available" $
      property $ \(x :: Int) -> x + 0 == x
