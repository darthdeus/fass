{-# LANGUAGE TemplateHaskell #-}
module Fass.Types where

import Control.Lens
import Data.Monoid
import Data.String
import qualified Data.Map as M

type Property = String
type Value = String

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
makePrisms ''Selector


-- data Entity = Ruleset Selector [Entity]
--             | Variable String String
--             | Rule Property Value
--             deriving (Show)

-- newtype CSSDocument = CSSDocument [CSSEntity]

-- data CSSEntity = CSSImport String
--                | CSSRuleset String
-- type Document = [Entity]
