module Evaluator where

data SASSEntity = SASSImport String
                | SASSVariable String String
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
data SASSRuleset = SASSRuleset [String]
                 deriving (Show)

compile :: SASSDocument -> CSSDocument
compile (SASSDocument entities) = CSSDocument $ map compileEntity entities

compileEntity :: SASSEntity -> CSSEntity
compileEntity (SASSImport file) = CSSImport file
compileEntity (SASSVariable name value) = CSSNoValue $ "TODO - figure out how to do variables " ++ name ++ value
compileEntity (SASSToplevelRuleset (SASSRuleset _)) = CSSNoValue "TODO - actually do stuff here"
--compileEntity value = CSSNoValue $ "TODO - figure out what this was" ++ show value
