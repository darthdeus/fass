module Fass.Parsers.SCSS where

import Control.Applicative ((*>), (<*))
import Control.Monad
import Fass.Types

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String

parseSCSS :: String -> Either ParseError [Entity]
parseSCSS = parse (many entity <* eof) "SCSS Parser"

entity :: Parser Entity
entity = do
    value <- try variable <|> try rule <|> ruleset'
    void spaces
    return value

entityList :: Parser [Entity]
entityList = many entity <* eof

paddedChar :: Char -> Parser ()
paddedChar c = void $ spaces >> char c >> spaces

ruleset' :: Parser Entity
ruleset' = do
    s <- selector'
    entities <- paddedChar '{' *> many entity <* paddedChar '}'
    return $ Nested (Ruleset s entities)

rule :: Parser Entity
rule = do
    void spaces
    property <- propertyName

    value <- paddedChar ':' *> propertyValue

    optional $ char ';'
    return $ Rule property value

selector' :: Parser Selector
selector' = do
    result <- many1 $ letter <|> oneOf " .#-_:>[]=" <|> digit
    return . Selector . T.unpack . T.strip . T.pack $ result

variable :: Parser Entity
variable = do
    name <- char '$' *> propertyName
    paddedChar ':'
    value <- propertyValue
    optional $ char ';'
    return $ Variable name value

propertyName :: Parser Property
propertyName = many1 $ letter <|> oneOf "_-*"

propertyValue :: Parser Value
propertyValue = many1 $ noneOf ";"
