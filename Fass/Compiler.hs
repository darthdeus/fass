module Fass.Compiler
  ( flatten
  , expandEntity )
  where

import           Fass.Types

flatten :: [Entity] -> [Entity]
flatten = flattenPrefix ""

flattenPrefix :: String -> [Entity] -> [Entity]
flattenPrefix prefix xs = concatMap unwrap $ expandSelectorWithPrefix prefix xs

expandSelectorWithPrefix :: String -> [Entity] -> [Entity]
expandSelectorWithPrefix prefix = concatMap (expandEntity prefix)

expandEntity :: String -> Entity -> [Entity]
expandEntity _ v@(Variable _ _) = [v]
expandEntity _ r@(Rule _ _) = [r]
expandEntity prefix (Ruleset s inner) = first : rest
    where first     = Ruleset newPrefix (filter (not . isRuleset) inner)
          rest      = flattenPrefix newPrefix (filter isRuleset inner)
          newPrefix = s ++ " " ++ prefix

unwrap :: Entity -> [Entity]
unwrap v@(Variable _ _) = [v]
unwrap r@(Rule _ _)     = [r]
unwrap r@(Ruleset _ inner) = r : filter isRuleset inner

isRuleset :: Entity -> Bool
isRuleset (Ruleset _ _) = True
isRuleset _             = False
