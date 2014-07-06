module Fass.Parser.Color where

import Control.Monad (void)
import Control.Applicative ((<$>))
import Text.Parsec
import Text.Parsec.String

data RGB = RGB Int Int Int
data RGBA = RGBA Int Int Int Float

-- TODO - remove duplication with Fass.Parser
paddedChar :: Char -> Parser ()
paddedChar c = void $ spaces >> char c >> spaces

number :: Parser Int
number = read <$> many1 digit

-- TODO - find a better way to express this
float :: Parser Float
float = do
    before <- many1 digit
    void $ char '.'
    after <- many1 digit

    return $ read $ before ++ "." ++ after

rgb :: Parser RGB
rgb = do
    void $ string "rgb("

    r <- number
    paddedChar ','
    g <- number
    paddedChar ','
    b <- number
    paddedChar ')'
    optional $ char ';'

    return $ RGB r g b

rgba :: Parser RGBA
rgba = do
    void $ string "rgba("

    r <- number
    paddedChar ','
    g <- number
    paddedChar ','
    b <- number
    paddedChar ','
    a <- float
    paddedChar ')'
    optional $ char ';'

    return $ RGBA r g b a

hexColorShort :: Parser String
hexColorShort = do
    void $ char '#'
    count 3 hexDigit
    -- TODO - force a space at the end

hexColorLong :: Parser String
hexColorLong = do
    void $ char '#'
    count 6 hexDigit
    -- TODO - force a space at the end

hexColorString :: Parser String
hexColorString = hexColorLong <|> hexColorShort
