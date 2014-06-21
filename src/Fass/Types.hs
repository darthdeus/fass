{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Fass.Types where

import Control.Lens
import Data.Monoid
import Data.String
import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Text as T

type Property = String
type Value = String

newtype Selector = Selector String deriving (Show, Eq)

instance Monoid Selector where
    mempty = Selector ""
    mappend (Selector x) (Selector y)
        | x == "" = Selector y
        | otherwise = Selector $ mungle x y

mungle :: String -> String -> String
mungle x y = intercalate ", " $ [ merge a b | a <- (splitOn "," x), b <- (splitOn "," y) ]
             where merge a b | '&' `elem` b = T.unpack $ T.replace "&" (T.pack a) (T.pack b)
                             | otherwise = a ++ " " ++ b

instance IsString Selector where
    fromString x = Selector x

data Ruleset = Ruleset Selector [Entity] deriving (Eq, Show)

data Entity = Variable String String
            | Comment String
            | Import FilePath
            | Rule String String
            | Nested Ruleset
            | Null
            deriving (Eq, Show)

type SASSEnv = M.Map String String

makePrisms ''Ruleset
makePrisms ''Entity
makePrisms ''Selector
