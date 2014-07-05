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

-- Since selectors are often concatenated together, it only makes sense to have a Monoid
-- instance encapsulating this behavior.
instance Monoid Selector where
    mempty = Selector ""
    mappend (Selector x) (Selector y)
        | x == "" = Selector y
        | otherwise = Selector $ mungle x y

-- Given two selectors (in String form) we need to merge them together in a way that
-- makes sense for the SASS nesting rules. This is also the place where '&' gets resolved
-- to the parent selector. Here are a few examples
--
-- "p" + "span" = "p span"
-- "p, h1" + "span, code" = "p span, h1 span, p code, h1 code"
-- "p" + "&:hover" = "p:hover"
--
mungle :: String -> String -> String
mungle x y = intercalate ", " $ [ merge a b | a <- (splitOn "," x), b <- (splitOn "," y) ]
             where merge a b | '&' `elem` b = T.unpack $ T.replace "&" (T.pack a) (T.pack b)
                             | otherwise = a ++ " " ++ b

-- While this isn't needed in the actual code, it's useful thing to have in tests.
-- TODO - maybe only extract this in a test helper module?
instance IsString Selector where
    fromString x = Selector x

data Ruleset = Ruleset Selector [Entity] deriving (Eq, Show)

data Entity = Variable String String
            | Comment String
            | Import FilePath
            | Rule String String
            | Nested Ruleset
            | Null
            -- ^ When doing some transformations on the AST, some Entities might get
            -- completely removed.  This might be the case of either variable
            -- inlining, comment removal or import resolution.  In such cases those
            -- Entities are replaced by the Null entity, which serves as a
            -- placeholder and gets later removed by the Printer.
            deriving (Eq, Show)

type SASSEnv = M.Map String String

makePrisms ''Ruleset
makePrisms ''Entity
makePrisms ''Selector
