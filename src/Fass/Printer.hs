module Fass.Printer where

import Fass.Types

prettyPrint :: [Ruleset] -> String
prettyPrint = concatMap printRuleset

printRuleset :: Ruleset -> String
printRuleset (Ruleset (Selector s) rules) = s ++ " {\n" ++ concatMap printRule rules ++ "}"

printRule :: Entity -> String
printRule (Rule key value) = "  " ++ key ++ ": " ++ value ++ "; "
printRule _ = ""
