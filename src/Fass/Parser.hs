module Fass.Parser where

import Fass.Types
import Control.Monad
import Control.Applicative ((*>), (<*))

import Text.Parsec
import Text.Parsec.String

eol :: GenParser Char st Char
eol = char '\n'

entity :: Parser SASSEntity
entity = do
    value <- try variable <|> try rule <|> ruleset
    void spaces
    return value

paddedChar :: Char -> Parser ()
paddedChar c = void $ spaces >> char c >> spaces

ruleset :: Parser SASSEntity
ruleset = do
    s <- selector
    entities <- paddedChar '{' *> many entity <* paddedChar '}'
    return $ SASSNestedRuleset (SASSRuleset s entities)

rule :: Parser SASSEntity
rule = do
    void spaces
    property <- propertyName

    void $ paddedChar ':'

    value <- propertyValue

    optional $ char ';'
    return $ SASSRule property value

selector :: Parser Selector
selector = do
    void spaces
    parsed <- many1 $ letter <|> oneOf " .#-_:>[]=" <|> digit

    if last parsed == ' '
        then return $ init parsed
        else return parsed

variable :: Parser SASSEntity
variable = do
    name <- char '$' *> propertyName
    paddedChar ':'
    value <- propertyValue
    optional $ char ';'
    return $ SASSVariable name value

propertyName :: Parser Property
propertyName = many1 $ letter <|> oneOf "-*"

propertyValue :: Parser Value
propertyValue = many1 $ noneOf ";"

parseSCSS :: String -> Either ParseError [SASSEntity]
parseSCSS = parse (many entity) "SCSS Parser"
