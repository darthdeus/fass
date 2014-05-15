module Fass.Printer where

import Fass.Types

prettyPrint :: [Entity] -> String
prettyPrint = concatMap prettyPrintEntity

prettyPrintEntity :: Entity -> String
prettyPrintEntity (Variable k v) = '$' : k ++ ":" ++ v ++ ";\n"
prettyPrintEntity (Rule k v) = k ++ ": " ++ v ++ ";\n"
prettyPrintEntity (Ruleset s inner) = s ++ " {\n" ++ prettyPrint inner ++ "}\n"
