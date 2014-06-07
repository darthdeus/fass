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
    property <- many1 $ letter <|> char '-'

    void $ paddedChar ':'
    -- TODO - make this more strict in terms of what can a value be
    value <- many1 $ letter <|> char '$' <|> char '#'
    optional $ char ';'
    return $ SASSRule property value

selector :: Parser Selector
selector = do
    spaces >> many letter

variable :: Parser SASSEntity
variable = do
    name <- char '$' *> propertyName
    paddedChar ':'
    value <- propertyValue
    optional $ char ';'
    return $ SASSVariable name value

propertyName :: Parser Property
propertyName = many1 $ letter <|> char '-'

propertyValue :: Parser Value
propertyValue = many1 $ letter <|> oneOf "-#"

parseSCSS :: String -> Either ParseError [SASSEntity]
parseSCSS = parse (many entity) "SCSS Parser"
