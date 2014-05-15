module Main where

-- import Text.Parsec
import Text.ParserCombinators.Parsec

eol :: GenParser Char st Char
eol = char '\n'

parseEntity :: Parser Entity
parseEntity = do
    value <- try parseRuleset <|> try parseRule <|> parseVariable
    many space
    return value

parseRuleset :: Parser Entity
parseRuleset = do
    selector <- parseSelector
    many space
    char '{'
    many space
    entities <- many parseEntity
    many space
    char '}'
    return $ Ruleset selector entities

parseRule :: Parser Entity
parseRule = do
    many space
    property <- many1 $ letter <|> char '-'
    many space
    char ':'
    many space
    -- TODO - make this more strict in terms of what can a value be
    value <- many1 $ letter <|> char '$' <|> char '#'
    optional $ char ';'
    return $ Rule property value

parseSelector :: Parser Selector
parseSelector = do
    many space
    many letter

parseVariable :: Parser Entity
parseVariable = do
    char '$'
    name <- parsePropertyName
    many $ char ' '
    char ':'
    many $ char ' '
    value <- parsePropertyValue
    optional $ char ';'
    return $ Variable name value

parsePropertyName :: Parser Property
parsePropertyName = many1 $ letter <|> char '-'

parsePropertyValue :: Parser Value
parsePropertyValue = many1 $ letter <|> char '-' <|> char '#'
    -- Not sure how to solve this prefix, since the return type in the
    -- monad isn't a string, so I can't concatenate it with the value
    -- itself
    -- prefix <- optional (char '#')


main = do
    text <- readFile "sample.scss"
    print $ parse (many parseEntity) "le parser" text

t = parse parseRuleset "some parser"

type Document = [Entity]

type Selector = String
type Property = String
type Value = String

data Entity = Ruleset Selector [Entity]
            | Variable String String
            | Rule Property Value
            deriving (Show)
