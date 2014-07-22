{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Fass.Compiler
       ( compile
       , compileFile
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
import           System.Directory
import           System.FilePath.Posix
import           Text.Regex

compileFile :: FilePath -> IO String
compileFile path = do
    content <- readFile path

    case parseSCSS content of
        Left err -> fail $ show err
        Right result -> compileEverything result $ takeDirectory path

compile :: String -> IO String
compile input = case parseSCSS input of
    -- TODO - handle errors
    Left err -> fail $ show err
    Right result -> getCurrentDirectory >>= compileEverything result

parseAndResolve :: FilePath -> String -> IO [Entity]
parseAndResolve baseDir content = case parseSCSS content of
    -- TODO - handle errors
    Left err -> fail $ show err
    Right result -> deepResolve baseDir result

-- TODO - run all of the import computations inside a Reader with the path
deepResolve :: FilePath -> [Entity] -> IO [Entity]
deepResolve baseDir entities = concat <$> mapM (inlineImportWithFile baseDir) entities

inlineImportWithFile :: FilePath -> Entity -> IO [Entity]
inlineImportWithFile baseDir (Import fileName) = do
    file <- readFile (baseDir </> fileName)
    parseAndResolve baseDir file

inlineImportWithFile baseDir (Nested (Ruleset s entities)) = do
    result <- concatMapM (inlineImportWithFile baseDir) entities
    return $ [Nested $ Ruleset s result]

inlineImportWithFile _ x = return [x]

compileEverything :: [Entity] -> FilePath -> IO String
compileEverything [] _ = return ""
compileEverything entities path = do
    importsDone <- concatMapM (inlineImportWithFile path) entities

    let inlined = flip evalState emptyEnv $ mapM inlineEntity importsDone

    let concatenated = concatMap (flatten "") $ inlined
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
minify = trim . collapseSpace . newlines

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
