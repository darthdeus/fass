module Main where

type Document = [Ruleset]

data Ruleset = Ruleset Selector [Rule]
             deriving (Show)

type Selector = String
type Property = String
type Value = String

data Rule = Declaration Property Value
          deriving (Show)
          -- | Variable Name Value


main = putStrLn "FASS IS HERE"
