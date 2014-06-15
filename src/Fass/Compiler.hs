module Fass.Compiler where

import Control.Monad.State
import Control.Lens
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
compileEverything entities =
    prettyPrint $ concatMap flatten $ rulesets

    where
      inlined :: [Entity]
      inlined = flip evalState emptyEnv $ mapM inlineEntity entities

      rulesets :: [Ruleset]
      rulesets = inlined ^.. traverse._Nested
