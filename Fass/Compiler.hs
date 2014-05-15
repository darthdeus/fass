module Fass.Compiler
  ( flatten
  , flattenEntity)
  where

import Fass.Types

flatten :: [Entity] -> [Entity]
flatten = flattenWithPrefix ""

flattenWithPrefix :: String -> [Entity] -> [Entity]
flattenWithPrefix prefix = map (flattenEntity prefix)

flattenEntity :: String -> Entity -> Entity
flattenEntity prefix v@(Variable _ _) = v
flattenEntity prefix r@(Rule _ _) = r
flattenEntity prefix (Ruleset s inner) = Ruleset newPrefix (flattenWithPrefix newPrefix inner)
              where newPrefix = s ++ " " ++ prefix
