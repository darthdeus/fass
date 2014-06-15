{-# LANGUAGE OverloadedStrings #-}
module Fass.Compiler where

import Control.Monad.State
import Control.Lens

import qualified Data.Text as T
import Text.Parsec (ParseError)

import Fass.Parsers.Sass (parseSass)
import Fass.Parsers.SCSS (parseSCSS)

import Fass.Evaluator
import Fass.Printer
import Fass.Types

compile :: Either ParseError [Entity] -> String
compile entities = case entities of Left err  -> fail $ show err
                                    Right res -> compileEverything res

compileEverything :: [Entity] -> String
compileEverything [] = ""
compileEverything entities = prettyPrint $ concatMap flatten rulesets
  where inlined :: [Entity]
        inlined = flip evalState emptyEnv $ mapM inlineEntity entities
        rulesets :: [Ruleset]
        rulesets = inlined ^.. traverse._Nested

compileFile :: String -> IO String
compileFile f = do
  code <- readFile f
  let parse = case last $ T.splitOn "." (T.pack f) of "sass" -> parseSass
                                                      _      -> parseSCSS
  return $ compile (parse code)
