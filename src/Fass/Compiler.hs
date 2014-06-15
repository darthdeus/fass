module Fass.Compiler where

import Fass.Parser
import Fass.Evaluator
import Fass.Printer
import Fass.Types

compile :: String -> String
compile input = case parseSCSS input of
    Left err -> fail $ show err
    Right result -> compileEverything result

compileEverything :: [Entity] -> String
compileEverything [] = ""
compileEverything entities = prettyPrint $ flatten entities
