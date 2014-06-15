{-# LANGUAGE TemplateHaskell #-}
module Fass.Types where

import Control.Lens
import Data.Monoid
import Data.String
import qualified Data.Map as M

type Property = String
type Value = String

-- data Entity = Ruleset Selector [Entity]
--             | Variable String String
--             | Rule Property Value
newtype Selector = Selector String deriving (Show, Eq)

instance Monoid Selector where
    mempty = Selector ""
    mappend (Selector x) (Selector y) = Selector (x ++ " " ++ y)

instance IsString Selector where
    fromString x = Selector x

data Ruleset = Ruleset Selector [Entity] deriving (Eq, Show)
data Entity = Variable String String
              | Rule String String
              | Nested Ruleset
              | Null
                deriving (Eq, Show)

type SASSEnv = M.Map String String

makePrisms ''Ruleset
makePrisms ''Entity


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
