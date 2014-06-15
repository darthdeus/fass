module Fass.Compiler where

import Control.Monad.State
import Control.Lens
import Fass.Evaluator
import Fass.Printer
import Fass.Types


import Fass.Parsers.Sass (parseSass)
import Fass.Parsers.SCSS (parseSCSS)

import Text.Parsec (ParseError)
import Data.List.Split
import Control.Lens
import Fass.Evaluator
import Fass.Printer
import Fass.Types

compile :: Either ParseError [Entity] -> String
compile entities = case entities of Left err  -> fail $ show err
                                    Right res -> compileEverything $ res ^.. traverse._Nested

compileEverything :: [Ruleset] -> String
compileEverything [] = ""
compileEverything entities = prettyPrint $ concatMap flatten entities

compileFile :: String -> IO String
compileFile f = do
  code <- readFile f
  let parse = case last $ splitOn "." f of "sass" -> parseSass
                                           _      -> parseSCSS
  return $ compile (parse code)
