{-# LANGUAGE OverloadedStrings #-}
module Fass.Compiler where

import qualified Data.Text as T
import Text.Parsec (ParseError)
import Control.Lens

import Fass.Parsers.Sass (parseSass)
import Fass.Parsers.SCSS (parseSCSS)

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
  let parse = case last $ T.splitOn "." (T.pack f) of "sass" -> parseSass
                                                      _      -> parseSCSS
  return $ compile (parse code)
