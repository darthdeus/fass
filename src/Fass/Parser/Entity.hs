module Fass.Parser.Entity where

import Fass.Types
import Fass.Parser.Color
import Fass.Parser.Helper

import Control.Applicative ((*>), (<*), (<$>))
import Control.Monad
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String

entity :: Parser Entity
entity = do
    value <- try comment <|> try importParser <|> try variable
             <|> try rule <|> ruleset
    spaces
    return value

ruleset :: Parser Entity
ruleset = do
    s <- selector
    entities <- paddedChar '{' *> many entity <* paddedChar '}'
    return $ Nested (Ruleset s entities)

rule :: Parser Entity
rule = do
    -- Rule can be indented, which is why we need to skip the leading spaces
    property <- spaces *> propertyName
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

importParser :: Parser Entity
importParser = fmap Import $ string "@import \"" *> many1 (noneOf "\"") <* char '"' <* optional (char ';')

propertyName :: Parser Property
propertyName = many1 $ letter <|> oneOf "_-*" <|> digit -- TODO - shouldn't accept digit as first character

propertyValue :: Parser Value
propertyValue = many1 $ noneOf ";"

entityList :: Parser [Entity]
entityList = many entity <* eof
