{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Fass.Compiler
       ( compile
       , compileEverything
       , minify
       , deepResolve
       , trim
       , inlineImportWithFile
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
    -- Handle the failure better
    Left err -> fail $ show err
    Right result -> compileEverything result

parseAndResolve :: String -> IO [Entity]
parseAndResolve content = case parseSCSS content of
    -- TODO - handle errors
    Left err -> fail $ show err
    Right result -> deepResolve result

deepResolve :: [Entity] -> IO [Entity]
deepResolve entities = concat <$> mapM inlineImportWithFile entities

inlineImportWithFile :: Entity -> IO [Entity]
inlineImportWithFile (Import fileName) = readFile fileName >>= parseAndResolve
inlineImportWithFile x = return [x]

compileEverything :: [Entity] -> IO String
compileEverything [] = return ""
compileEverything entities = do
    -- TODO - the imports probably need to be resolved before resolving
    -- variables, since new variables an be imported, or there can be dependencies
    -- on the existing ones.
    let inlined = flip evalState emptyEnv $ mapM inlineEntity entities

    importsDone <- (traverse._Nested._Ruleset._2) (concatMapM inlineImportWithFile) inlined

    let concatenated = concatMap (flatten "") $ importsDone
    return . prettyPrint $ compactSelectors concatenated

-- TODO - why does this need NoMonomorphismRestriction to work with no type
compactSelectors :: forall (t :: * -> *). Traversable t => t Entity -> t Entity
compactSelectors = over (traverse._Nested._Ruleset._1._Selector) compactSelector

concatMapM :: (Monad m, Functor m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

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
