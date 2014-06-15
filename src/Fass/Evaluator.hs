module Fass.Evaluator where

import Fass.Types

import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Data.List
import Control.Applicative ((<$>))
import Control.Lens

emptyEnv :: SASSEnv
emptyEnv = M.empty

inlineVariables :: Ruleset -> State SASSEnv Ruleset
inlineVariables (Ruleset s []) = return $ Ruleset s []
inlineVariables (Ruleset s entities) = mapM inlineEntity entities >>= return . Ruleset s

inlineEntity :: Entity -> State SASSEnv Entity
inlineEntity x = case x of
            Variable name value -> modify (M.insert name value) >> return Null
            Rule name value -> Rule name <$> expandValue value
            Nested _ -> return Null
            _ -> return Null

expandValue :: String -> State SASSEnv String
expandValue value = if isVariableName value
                    then liftM (fromJust . M.lookup (tail value)) get
                    else return value

isVariableName :: String -> Bool
isVariableName ('$':_) = True
isVariableName _ = False

unwrap :: [Entity] -> [Ruleset]
unwrap entities = concatMap flatten $ entities ^.. traverse._Nested

flatten :: Ruleset -> [Ruleset]
flatten x@(Ruleset _ []) = [x]
flatten (Ruleset s entities) = if null others then unwrapped
                               else Ruleset s others : unwrapped
  where (nested, others) = partition (not . isn't _Nested) entities
        unwrapped = concatMap (moreFlatten s) nested

moreFlatten :: Selector -> Entity -> [Ruleset]
moreFlatten psel (Nested (Ruleset nsel xs)) = [Ruleset (psel <> nsel) xs]
moreFlatten _ _ = []
