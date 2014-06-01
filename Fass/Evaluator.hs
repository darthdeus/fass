module Fass.Evaluator where

import           Control.Monad.State
import qualified Data.Map            as M
import Data.Maybe
import Control.Applicative ((<$>))

data SASSRuleset = SASSRuleset [SASSEntity]
                   deriving (Eq, Show)

data SASSEntity = SASSVariable String String
                | SASSRule String String
                | SASSNothing
                  deriving (Eq, Show)

type SASSEnv = M.Map String String

emptyEnv :: SASSEnv
emptyEnv = M.empty

compile :: SASSRuleset -> State SASSEnv SASSRuleset
compile (SASSRuleset entities) = liftM SASSRuleset $ compileEntities entities

compileEntities :: [SASSEntity] -> State SASSEnv [SASSEntity]
compileEntities xs = filter (/= SASSNothing) <$> forM xs compileEntity

compileEntity :: SASSEntity -> State SASSEnv SASSEntity
compileEntity SASSNothing = return SASSNothing
compileEntity (SASSVariable name value) = modify (M.insert name value) >> return SASSNothing
compileEntity (SASSRule name value) = SASSRule name <$> expandValue value

expandValue :: String -> State SASSEnv String
expandValue value = if isVariableName value
                    then get >>= return . fromJust . M.lookup (tail value)
                    else return value

isVariableName :: String -> Bool
isVariableName ('$':_) = True
isVariableName _ = False

-- test :: [SASSEntity]
-- test = compileEntities emptyEnv exampleData

exampleData :: [SASSEntity]
exampleData = [SASSVariable "light-text" "#fafafa", SASSVariable "dark-text" "#0f0f0", SASSRule "color" "$light-text"]
