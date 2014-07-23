{-# LANGUAGE OverloadedStrings #-}
module Fass.Evaluator
    ( inlineEntity
    , inlineVariables
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

-- Flattens all nested entities inside a given entity with a given selector prefix.
-- This comes handy when unwrapping the following structure
--
-- p {
--   span { color: red; }
-- }
--
-- When evaluating the nested `span`, this function would receive `p` as a prefix selector,
-- which would then result in a `p span` selector when combined together.
flatten :: Selector -> Entity -> [Entity]
flatten prefix (Nested (Ruleset s entities)) =
    if null rules then unwrapped
    else Nested (Ruleset (prefix <> s) rules) : unwrapped
  where
    (rules, nested) = partition (isn't _Nested) entities
    unwrapped = concatMap (flatten (prefix <> s)) $ nested
flatten _ Null = []
flatten _ a = [a]

inlineVariables :: Ruleset -> State SASSEnv Ruleset
inlineVariables (Ruleset s []) = return $ Ruleset s []
inlineVariables (Ruleset s entities) = mapM inlineEntity entities >>= return . Ruleset s

inlineEntity :: Entity -> State SASSEnv Entity
inlineEntity x = case x of
            Variable name value -> do
                val <- expandValue value
                modify (M.insert name val)
                return Null
            Rule name value -> Rule name <$> expandValue value
            Nested ruleset -> do
                current <- get
                return . Nested . flip evalState current $ inlineVariables ruleset
            Comment c -> return $ Comment c
            _ -> return Null

-- Replaces all occurences of variables in a given String. Here are a few examples
--
--  $header-bg: #fafafa;
--
--  color: $header-bg;             -> color: #fafafa;
--  border: 1px solid $header-bg;  -> border: 1px solid #fafafa;
--
-- It is important to note here, that the variable might only be a part of the property value,
-- not just the simple case where the whole value is represented by a variable.
expandValue :: String -> State SASSEnv String
expandValue value = do
    env <- get
    return . unwords . map (variableValue env) $ words value

    where
      variableValue :: SASSEnv -> String -> String
      -- TODO - better error handling in case the variable name is missgin
      variableValue env ('$':v) = maybe "" id $ M.lookup v env
      variableValue _ v = v

-- TODO - document what this actually does
unwrap :: [Entity] -> [Entity]
unwrap = concatMap (flatten "")
