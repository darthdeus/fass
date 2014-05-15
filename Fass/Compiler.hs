module Fass.Compiler
  ( flatten
  , expandSelector )
  where

import           Fass.Types

flatten :: [Entity] -> [Entity]
flatten xs = concatMap unwrap $ expandSelectorWithPrefix "" xs

expandSelectorWithPrefix :: String -> [Entity] -> [Entity]
expandSelectorWithPrefix prefix = map (expandSelector prefix)

expandSelector :: String -> Entity -> Entity
expandSelector prefix v@(Variable _ _) = v
expandSelector prefix r@(Rule _ _) = r
expandSelector prefix (Ruleset s inner) = Ruleset newPrefix (expandSelectorWithPrefix newPrefix inner)
                 where newPrefix = s ++ " " ++ prefix

unwrap :: Entity -> [Entity]
unwrap v@(Variable _ _) = [v]
unwrap r@(Rule _ _)     = [r]
unwrap r@(Ruleset _ inner) = r : filter isRuleset inner

isRuleset :: Entity -> Bool
isRuleset (Ruleset _ _) = True
isRuleset _             = False
