module Fass.Printer where

import Fass.Types

prettyPrint :: [SASSEntity] -> String
prettyPrint = concatMap prettyPrintEntity

prettyPrintEntity :: SASSEntity -> String
prettyPrintEntity SASSNothing = ""
prettyPrintEntity (SASSVariable k v) = '$' : k ++ ":" ++ v ++ ";\n"
prettyPrintEntity (SASSRule k v) = k ++ ": " ++ v ++ ";\n"
prettyPrintEntity (SASSNestedRuleset (SASSRuleset s inner)) =
    s ++ " {\n" ++ prettyPrint inner ++ "}\n"
