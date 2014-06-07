module Fass.Parser where

import Control.Applicative ((*>), (<*))
import Control.Monad
import Fass.Types

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String

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
    result <- many1 $ letter <|> oneOf " .#-_:>[]=" <|> digit
    return . T.unpack . T.strip . T.pack $ result

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
parseSCSS = parse (many entity <* eof) "SCSS Parser"
