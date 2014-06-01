module Fass.Types where

import qualified Data.Map as M

-- type Document = [Entity]

type Selector = String
type Property = String
type Value = String

data Entity = Ruleset Selector [Entity]
            | Variable String String
            | Rule Property Value
            deriving (Show)

newtype CSSDocument = CSSDocument [CSSEntity]

data CSSEntity = CSSImport String
               | CSSRuleset String
