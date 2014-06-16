module Fass.Parser where

import Control.Applicative ((*>), (<*), (<$>))
import Control.Monad
import Fass.Types

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String

entity :: Parser Entity
entity = do
    value <- try comment <|> try variable <|> try rule <|> ruleset
    spaces
    return value

paddedChar :: Char -> Parser ()
paddedChar c = void $ spaces >> char c >> spaces

ruleset :: Parser Entity
ruleset = do
    s <- selector
    entities <- paddedChar '{' *> many entity <* paddedChar '}'
    return $ Nested (Ruleset s entities)

rule :: Parser Entity
rule = do
    void spaces
    property <- propertyName

    value <- paddedChar ':' *> propertyValue

    optional $ char ';'
    return $ Rule property value

selector :: Parser Selector
selector = do
    result <- many1 $ letter <|> oneOf " .,~+#*-_:>[]='\"()&" <|> digit
    return . Selector . T.unpack . T.strip . T.pack $ result

comment :: Parser Entity
comment = try blockComment <|> lineComment

lineComment :: Parser Entity
lineComment = string "//" >> many (noneOf "\n") >> (void (char '\n') <|> eof) >> return Null

blockComment :: Parser Entity
blockComment = Comment <$> (string "/*" *> many (noneOf "*") <* string "*/")

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

entityList :: Parser [Entity]
entityList = many entity <* eof

parseSCSS :: String -> Either ParseError [Entity]
parseSCSS = parse entityList "SCSS Parser"
