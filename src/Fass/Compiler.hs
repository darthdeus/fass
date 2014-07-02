{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Fass.Compiler
       ( compile
       , compileEverything
       ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Char (isSpace)
import qualified Data.Text as T
import           Fass.Evaluator
import           Fass.Parser
import           Fass.Printer
import           Fass.Types
import           Text.Regex

compile :: String -> IO String
compile input = case parseSCSS input of
    Left err -> fail $ show err
    Right result -> compileEverything result

compileEverything :: [Entity] -> IO String
compileEverything [] = return ""
compileEverything entities = do
    let inlined = flip evalState emptyEnv $ mapM inlineEntity entities

    importsDone <- (traverse._Nested._Ruleset._2) (concatMapM inlineImportWithFile) inlined

    let concatenated = concatMap (flatten "") $ importsDone
    return . prettyPrint $ compactSelectors concatenated

-- TODO - why does this need NoMonomorphismRestriction to work with no type
compactSelectors :: forall (t :: * -> *). Traversable t => t Entity -> t Entity
compactSelectors = over (traverse._Nested._Ruleset._1._Selector) compactSelector

concatMapM :: (Monad f, Functor f) => (a -> f [b]) -> [a] -> f [b]
concatMapM f xs = fmap concat (mapM f xs)

inlineImportWithFile :: Entity -> IO [Entity]
inlineImportWithFile (Import fileName) = do
    content <- readFile fileName

    case parseSCSS content of
        Left err -> fail $ show err
        Right result -> return result
inlineImportWithFile x = return [x]

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
