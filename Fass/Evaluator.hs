module Fass.Evaluator where

import           Control.Monad.State
import qualified Data.Map            as M
import Data.Maybe

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
compile (SASSRuleset []) = return $ SASSRuleset []
compile (SASSRuleset [x]) = compileEntity x >>= return . SASSRuleset . (:[])
compile (SASSRuleset (x:y:_)) = do
  cx <- compileEntity x
  cy <- compileEntity y
  return $ SASSRuleset [cx, cy]

compileEntity :: SASSEntity -> State SASSEnv SASSEntity
compileEntity (SASSVariable name value) = modify (M.insert name value) >> return SASSNothing
compileEntity SASSNothing = return SASSNothing
compileEntity (SASSRule name value) = do
  s <- get
  let expandedValue = evalState (expandValue value) s
  return $ SASSRule name expandedValue


expandValue :: String -> State SASSEnv String
expandValue value = if isVariableName value
                    then get >>= return . fromJust . M.lookup (tail value)
                    else return value

isVariableName :: String -> Bool
isVariableName ('$':_) = True
isVariableName _ = False

justs :: [Maybe a] -> [a]
justs [] = []
justs (Nothing:xs) = justs xs
justs ((Just x):xs) = x : justs xs

test :: [SASSEntity]
test = filter x results
    where x SASSNothing = False
          x _ = True

          SASSRuleset results = flip evalState emptyEnv $ compile $ exampleData

exampleData = SASSRuleset [SASSVariable "olaf" "#fafafa", SASSRule "color" "$olaf"]
