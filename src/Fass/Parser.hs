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

parseEntity :: Parser SASSEntity
parseEntity = do
    value <- try parseVariable <|> try parseRule <|> parseRuleset
    void spaces
    return value

paddedChar :: Char -> Parser ()
paddedChar c = void $ spaces >> char c >> spaces

parseRuleset :: Parser SASSEntity
parseRuleset = do
    selector <- parseSelector
    entities <- paddedChar '{' *> many parseEntity <* paddedChar '}'
    return $ SASSNestedRuleset (SASSRuleset selector entities)

parseRule :: Parser SASSEntity
parseRule = do
    void spaces
    property <- many1 $ letter <|> char '-'

    void $ paddedChar ':'
    -- TODO - make this more strict in terms of what can a value be
    value <- many1 $ letter <|> char '$' <|> char '#'
    optional $ char ';'
    return $ SASSRule property value

parseSelector :: Parser Selector
parseSelector = do
    spaces >> many letter

parseVariable :: Parser SASSEntity
parseVariable = do
    name <- char '$' *> parsePropertyName
    paddedChar ':'
    value <- parsePropertyValue
    optional $ char ';'
    return $ SASSVariable name value

parsePropertyName :: Parser Property
parsePropertyName = many1 $ letter <|> char '-'

parsePropertyValue :: Parser Value
parsePropertyValue = many1 $ letter <|> oneOf "-#"

parseSCSS :: String -> Either ParseError [SASSEntity]
parseSCSS = parse (many parseEntity) "SCSS Parser"
