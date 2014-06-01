module Fass.Evaluator where

import Fass.Types

import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import Control.Applicative ((<$>))

emptyEnv :: SASSEnv
emptyEnv = M.empty

compile :: SASSRuleset -> State SASSEnv SASSRuleset
compile (SASSRuleset selector entities) = liftM (SASSRuleset selector) $ compileEntities entities

compileEntities :: [SASSEntity] -> State SASSEnv [SASSEntity]
compileEntities xs = filter (/= SASSNothing) <$> forM xs compileEntity

compileEntity :: SASSEntity -> State SASSEnv SASSEntity
compileEntity SASSNothing = return SASSNothing
compileEntity (SASSVariable name value) = modify (M.insert name value) >> return SASSNothing
compileEntity (SASSRule name value) = SASSRule name <$> expandValue value

expandValue :: String -> State SASSEnv String
expandValue value = if isVariableName value
                    then liftM (fromJust . M.lookup (tail value)) get
                    else return value

isVariableName :: String -> Bool
isVariableName ('$':_) = True
isVariableName _ = False

test :: [SASSEntity]
test = flip evalState emptyEnv $ compileEntities exampleData

exampleData :: [SASSEntity]
exampleData = [SASSVariable "light-text" "#fafafa", SASSVariable "dark-text" "#0f0f0", SASSRule "color" "$light-text"]
