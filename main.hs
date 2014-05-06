module Main where

-- import Text.Parsec
import Text.ParserCombinators.Parsec

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

eol :: GenParser Char st Char
eol = char '\n'

parseRuleset = do
    selector <- parseSelector
    many space
    char '{'
    rules <- many parseRule
    char '}'
    many eol
    return $ Ruleset selector rules

parseSelector = many letter

parseRule :: Parser Rule
parseRule = do
    many space
    property <- many1 $ letter <|> char '-'
    many space
    char ':'
    many space
    value <- many1 letter
    optional $ char ';'
    many eol
    return $ Declaration property value

main = do
    text <- readFile "sample.scss"
    print $ parse (many parseRuleset) "le parser" text

type Document = [Ruleset]

data Ruleset = Ruleset Selector [Rule]
             deriving (Show)

type Selector = String
type Property = String
type Value = String

data Rule = Declaration Property Value
          deriving (Show)
          -- | Variable Name Value

