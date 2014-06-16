{-# LANGUAGE OverloadedStrings #-}
module Fass.Compiler where

import Control.Lens
import Control.Monad.State
import Data.Char (isSpace)
import qualified Data.Text as T
import Fass.Evaluator
import Fass.Parser
import Fass.Printer
import Fass.Types
import Text.Regex

compile :: String -> String
compile input = case parseSCSS input of
    Left err -> fail $ show err
    Right result -> compileEverything result

compileEverything :: [Entity] -> String
compileEverything [] = ""
compileEverything entities =
    prettyPrint $ over (traverse._Nested._Ruleset._1._Selector) compactSelector $ concatMap (flatten "") $ inlined

    where
      inlined :: [Entity]
      inlined = flip evalState emptyEnv $ mapM inlineEntity entities

compactSelector :: String -> String
compactSelector s = T.unpack $ r " )" ")" $ r "( " "(" $ r " ]" "]" $
                    T.pack $ rep " +" " " $ rep " +\\*= +" "*=" $ rep " += +" "=" s
  where
    r = T.replace
    rep what with x = subRegex (mkRegex what) x with

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
