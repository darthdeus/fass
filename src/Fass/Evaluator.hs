{-# LANGUAGE OverloadedStrings #-}
module Fass.Evaluator
    ( inlineEntity
    , inlineVariables
    , inlineList
    , emptyEnv
    , unwrap
    , flatten
    ) where

import Fass.Types

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid
import Data.List

emptyEnv :: SASSEnv
emptyEnv = M.empty

inlineList :: [Ruleset] -> [Ruleset]
inlineList xs = flip evalState emptyEnv $ mapM inlineVariables xs

inlineVariables :: Ruleset -> State SASSEnv Ruleset
inlineVariables (Ruleset s []) = return $ Ruleset s []
inlineVariables (Ruleset s entities) = mapM inlineEntity entities >>= return . Ruleset s

inlineEntity :: Entity -> State SASSEnv Entity
inlineEntity x = case x of
            Variable name value -> do
                val <- expandValue value
                modify (M.insert name val) >> return Null
            Rule name value -> Rule name <$> expandValue value
            Nested ruleset -> do
                current <- get
                return . Nested . flip evalState current $ inlineVariables ruleset
            Comment c -> return $ Comment c
            _ -> return Null

expandValue :: String -> State SASSEnv String
expandValue value = do
    env <- get
    return . unwords . map (inlineVariable env) $ words value

inlineVariable :: SASSEnv -> String -> String
inlineVariable env ('$':value) = maybe "" id $ M.lookup value env
inlineVariable _ value = value

unwrap :: [Entity] -> [Entity]
unwrap = concatMap (flatten "")

flatten :: Selector -> Entity -> [Entity]
flatten prefix (Nested (Ruleset s entities)) = if null rules then unwrapped
                                               else Nested (Ruleset (prefix <> s) rules) : unwrapped
  where
    (rules, nested) = partition (isn't _Nested) entities
    unwrapped = concatMap (flatten (prefix <> s)) $ nested
flatten _ Null = []
flatten _ a = [a]
