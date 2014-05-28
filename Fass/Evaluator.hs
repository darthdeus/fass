module Fass.Evaluator where

import qualified Data.Map as M

data SASSEntity = SASSImport String
                | SASSVariable Binding
                | SASSToplevelRuleset SASSRuleset
                  deriving (Show)

data CSSEntity = CSSImport String
               | CSSRuleset Selector [CSSRule]
               | CSSNoValue String
                 deriving (Show)

type Selector = String
type CSSRule = [String]
newtype CSSDocument = CSSDocument [CSSEntity]

newtype SASSDocument = SASSDocument [SASSEntity]
data SASSRuleset = SASSRuleset [String] deriving (Show)

type SASSEnv = [Binding]
type Bindings = M.Map String String

emptyEnv :: SASSEnv
emptyEnv = []

compile :: SASSDocument -> CSSDocument
compile SASSDocument [] = []
compile SASSDocument (x:xs) =
    runReader emptyEnv $ do
      case x of
        SASSImport s -> s
        SASSVariable (name, value) -> ask >>= local (M.insert name value)











-- compile :: SASSDocument -> CSSDocument
-- compile (SASSDocument entities) = CSSDocument $ map compileEntity entities

compileEntity :: SASSEntity -> CSSEntity
compileEntity (SASSImport file) = CSSImport file
compileEntity (SASSVariable name value) = CSSNoValue $ "TODO - figure out how to do variables " ++ name ++ value
compileEntity (SASSToplevelRuleset (SASSRuleset _)) = CSSNoValue "TODO - actually do stuff here"
--compileEntity value = CSSNoValue $ "TODO - figure out what this was" ++ show value
