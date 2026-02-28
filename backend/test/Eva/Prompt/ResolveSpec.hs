{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for EVA-98: Eva.Prompt.Resolve — pure template resolution.
-- No DB or AppM needed; all tests run directly on the pure function.
module Eva.Prompt.ResolveSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Eva.Prompt.Resolve (resolveTemplate)

spec :: Spec
spec = do
  describe "resolveTemplate" $ do

    it "substitutes a single variable" $ do
      resolveTemplate "Hello {{name}}" (Map.fromList [("name", "World")])
        `shouldBe` ("Hello World", [])

    it "substitutes multiple variables" $ do
      resolveTemplate "{{greeting}} {{name}}"
        (Map.fromList [("greeting", "Hello"), ("name", "Alice")])
        `shouldBe` ("Hello Alice", [])

    it "returns the original text and the variable name when binding is missing" $ do
      resolveTemplate "Hello {{name}}" Map.empty
        `shouldBe` ("Hello {{name}}", ["name"])

    it "reports each unresolved variable only once even if repeated" $ do
      resolveTemplate "{{x}} and {{x}}" Map.empty
        `shouldBe` ("{{x}} and {{x}}", ["x"])

    it "passes through text with no markers unchanged" $ do
      resolveTemplate "static output" Map.empty
        `shouldBe` ("static output", [])

    it "handles nested braces {{{var}}} safely without crashing" $ do
      -- "{{{var}}}" starts with "{{", so the parsed variable name is "{var" (with a
      -- leading brace). Since "{var" has no binding the marker is kept verbatim.
      -- The important property is that this never throws.
      let (out, _unresolved) = resolveTemplate "{{{var}}}" (Map.fromList [("var", "X")])
      T.null out `shouldBe` False

    it "passes through an unclosed {{ verbatim without crashing" $ do
      let (out, unresolved) = resolveTemplate "open {{ no close" Map.empty
      T.isInfixOf "{{" out `shouldBe` True
      unresolved `shouldBe` []
