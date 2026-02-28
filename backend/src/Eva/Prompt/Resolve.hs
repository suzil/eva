{-# LANGUAGE OverloadedStrings #-}

-- | Pure template resolution: replaces @{{variableName}}@ markers in a text
-- using a caller-supplied binding map. Never throws — missing bindings are
-- kept as their literal @{{name}}@ form and reported in the unresolved list.
module Eva.Prompt.Resolve
  ( resolveTemplate
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Resolve all @{{variableName}}@ markers in @tmpl@ using @bindings@.
--
-- Returns @(resolvedText, unresolvedNames)@:
--
-- * @resolvedText@ — the input with every bound variable substituted.
--   Variables with no entry in @bindings@ are left as their literal
--   @{{name}}@ form so the output is always well-formed text.
-- * @unresolvedNames@ — the names of every variable that had no binding,
--   in order of first occurrence.  Empty when all markers were resolved.
--
-- Unclosed @{{@ sequences are passed through verbatim (they cannot be
-- resolved and are not reported as unresolved variable names).
resolveTemplate :: Text -> Map Text Text -> (Text, [Text])
resolveTemplate tmpl bindings = go "" [] tmpl
  where
    go acc unresolved t =
      case T.breakOn "{{" t of
        (prefix, "")   -> (acc <> prefix, reverse unresolved)
        (prefix, rest) ->
          case T.breakOn "}}" (T.drop 2 rest) of
            -- Unclosed "{{" — pass through verbatim, stop scanning.
            (_, "")           -> (acc <> prefix <> T.drop 0 rest, reverse unresolved)
            (varName, suffix) ->
              let key   = T.strip varName
                  after = T.drop 2 suffix
              in case Map.lookup key bindings of
                   Just val ->
                     go (acc <> prefix <> val) unresolved after
                   Nothing  ->
                     -- Keep the literal marker and record the unresolved name
                     -- (deduplicate: only add if not already recorded).
                     let unresolved' =
                           if key `elem` unresolved
                             then unresolved
                             else key : unresolved
                     in go (acc <> prefix <> "{{" <> key <> "}}") unresolved' after
