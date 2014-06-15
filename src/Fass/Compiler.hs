{-# LANGUAGE OverloadedStrings #-}
module Fass.Compiler where

import Control.Monad.State
import Data.Char (isSpace)
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
    prettyPrint $ concatMap (flatten "") $ inlined

    where
      inlined :: [Entity]
      inlined = flip evalState emptyEnv $ mapM inlineEntity entities

minify :: String -> String
minify css =
    trim
    $ collapseSpace
    $ newlines css

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

collapseSpace :: String -> String
collapseSpace [] = []
collapseSpace (' ':' ':xs) = collapseSpace (' ':xs)
collapseSpace (';':' ':xs) = collapseSpace (';':xs)
collapseSpace ('{':' ':xs) = collapseSpace ('{':xs)
collapseSpace ('}':' ':xs) = collapseSpace ('}':xs)
collapseSpace (x:xs) = x:collapseSpace xs

newlines :: String -> String
newlines [] = []
newlines ('\n':xs) = ' ':(newlines xs)
newlines (x:xs) = x:(newlines xs)
