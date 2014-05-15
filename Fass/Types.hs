module Fass.Types
  ( Document
  , Selector
  , Property
  , Value
  , Entity(..))
where

type Document = [Entity]

type Selector = String
type Property = String
type Value = String

data Entity = Ruleset Selector [Entity]
            | Variable String String
            | Rule Property Value
            deriving (Show)

