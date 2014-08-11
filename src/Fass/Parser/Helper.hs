module Fass.Parser.Helper where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Text.Parsec
import Text.Parsec.String

paddedChar :: Char -> Parser ()
paddedChar c = void $ spaces >> char c >> spaces

floatNumber :: Parser Float
floatNumber = try float <|> (fromIntegral <$> number)

number :: Parser Int
number = read <$> many1 digit

-- TODO - find a better way to express this
float :: Parser Float
float = do
    before <- many1 digit
    void $ char '.'
    after <- many1 digit

    return $ read $ before ++ "." ++ after
