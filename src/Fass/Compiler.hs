module Fass.Compiler where

import Control.Lens
import Fass.Parser
import Fass.Evaluator
import Fass.Printer
import Fass.Types

compile :: String -> String
compile input = case parseSCSS input of
    Left err -> fail $ show err
    Right result -> compileEverything $ result ^.. traverse._Nested

compileEverything :: [Ruleset] -> String
compileEverything [] = ""
compileEverything entities = prettyPrint $ concatMap flatten entities
