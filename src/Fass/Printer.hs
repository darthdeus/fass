module Fass.Printer where

import Fass.Types

prettyPrint :: [Entity] -> String
prettyPrint = concatMap prettyPrintEntity

prettyPrintEntity :: Entity -> String
prettyPrintEntity Null = ""
prettyPrintEntity (Variable k v) = '$' : k ++ ":" ++ v ++ ";\n"
prettyPrintEntity (Rule k v) = k ++ ": " ++ v ++ ";\n"
prettyPrintEntity (Nested (Ruleset (Selector s) inner)) =
    s ++ " {\n" ++ prettyPrint inner ++ "}\n"
