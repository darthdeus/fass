module Fass.Parser
  (parseSCSS)
where

import Fass.Types
import Control.Monad
import Control.Applicative ((*>), (<*))

-- import Text.Parsec
import Text.ParserCombinators.Parsec

eol :: GenParser Char st Char
eol = char '\n'

parseEntity :: Parser Entity
parseEntity = do
    value <- try parseVariable <|> try parseRule <|> parseRuleset
    void spaces
    return value

paddedChar :: Char -> Parser ()
paddedChar c = void $ spaces >> char c >> spaces

parseRuleset :: Parser Entity
parseRuleset = do
    selector <- parseSelector
    entities <- paddedChar '{' *> many parseEntity <* paddedChar '}'
    return $ Ruleset selector entities

parseRule :: Parser Entity
parseRule = do
    void spaces
    property <- many1 $ letter <|> char '-'

    void $ paddedChar ':'
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


parseSCSS :: String -> Either ParseError [Entity]
parseSCSS = parse (many parseEntity) "SCSS Parser"
