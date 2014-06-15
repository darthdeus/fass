{-# LANGUAGE TemplateHaskell #-}
module Fass.Types where

import qualified Data.Map as M

-- type Document = [Entity]

type Selector = String
type Property = String
type Value = String

-- data Entity = Ruleset Selector [Entity]
--             | Variable String String
--             | Rule Property Value
--             deriving (Show)

-- newtype CSSDocument = CSSDocument [CSSEntity]

-- data CSSEntity = CSSImport String
--                | CSSRuleset String


data SASSRuleset = SASSRuleset Selector [SASSEntity]
                   deriving (Eq, Show)

data SASSEntity = SASSVariable String String
                | SASSRule String String
                | SASSNestedRuleset SASSRuleset
                | SASSNothing
                  deriving (Eq, Show)

type SASSEnv = M.Map String String
