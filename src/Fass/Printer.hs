module Fass.Printer where

import Fass.Types
import Data.List

prettyPrint :: [Ruleset] -> String
prettyPrint rulesets = intercalate "\n\n" $ map printRuleset rulesets

printRuleset :: Ruleset -> String
printRuleset (Ruleset (Selector s) rules) = s ++ " {\n" ++ printedRules rules ++ " }"
  where
    printedRules :: [Entity] -> String
    printedRules xs = intercalate "\n" $ map (printRule 2) xs

printRule :: Int -> Entity -> String
printRule indentation (Rule key value) = replicate indentation ' ' ++ key ++ ": " ++ value ++ ";"
printRule _ _ = ""
