module Fass.Evaluator where

import Fass.Types

import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import Control.Applicative ((<$>))

emptyEnv :: SASSEnv
emptyEnv = M.empty

inlineVariables :: SASSRuleset -> State SASSEnv SASSRuleset
inlineVariables (SASSRuleset s []) = return $ SASSRuleset s []
inlineVariables (SASSRuleset s entities) = mapM inlineEntity entities >>= return . SASSRuleset s

inlineEntity :: SASSEntity -> State SASSEnv SASSEntity
inlineEntity x = case x of
            SASSVariable name value -> modify (M.insert name value) >> return SASSNothing
            SASSRule name value -> SASSRule name <$> expandValue value
            SASSNestedRuleset _ -> return SASSNothing
            _ -> return SASSNothing

expandValue :: String -> State SASSEnv String
expandValue value = if isVariableName value
                    then liftM (fromJust . M.lookup (tail value)) get
                    else return value

isVariableName :: String -> Bool
isVariableName ('$':_) = True
isVariableName _ = False
