module Fass.Parsers.Sass where

import Control.Applicative ((*>), (<*))
import Control.Monad
import Fass.Types

import qualified Data.Text as T
import Text.Parsec hiding (State, parse)
import Text.Parsec.Indent
import Control.Monad.State

type Parser a = ParsecT String () (State SourcePos) a

parseSass :: String -> Either ParseError [Entity]
parseSass i = runIndent "Sass Parser" $ runParserT entityList () "SassParser" i

entity :: Parser Entity
entity = do
    void spaces
    value <- try variable <|> try rule <|> ruleset'
    void spaces
    return value

entityList :: Parser [Entity]
entityList = many entity <* eof

paddedChar :: Char -> Parser ()
paddedChar c = void $ spaces >> char c >> spaces

ruleset' :: Parser Entity
ruleset' = do
  rs <- withBlock Ruleset selector' entity
  return $ Nested rs

rule :: Parser Entity
rule = do
    property <- propertyName
    value <- paddedChar ':' *> propertyValue
    return $ Rule property value

selector' :: Parser Selector
selector' = do
    result <- many1 $ letter <|> oneOf " .#-_:>[]=" <|> digit
    void spaces
    return . Selector . T.unpack . T.strip . T.pack $ result

variable :: Parser Entity
variable = do
    name <- char '$' *> propertyName
    paddedChar ':'
    value <- propertyValue
    return $ Variable name value

propertyName :: Parser Property
propertyName = many1 $ letter <|> oneOf "_-*"

propertyValue :: Parser Value
propertyValue = many1 $ noneOf "\n"
