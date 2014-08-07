module Fass.Parser.Color
       ( RGBA(..)
       , colorParser
       ) where

import Fass.Parser.Helper

import Control.Monad (void)
import Control.Applicative ((<$>))
import Data.List.Split
import Data.Maybe
import Numeric
import Text.Parsec
import Text.Parsec.String

data RGBA = RGBA Int Int Int Float deriving (Show, Eq)
data RGB  = RGB Int Int Int deriving (Show, Eq)

convertRGB :: RGB -> RGBA
convertRGB (RGB r g b) = RGBA r g b 0

colorParser :: Parser RGBA
colorParser = try hexColor <|> try (convertRGB <$> rgb) <|> rgba

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

hexColor :: Parser RGBA
hexColor = fmap convertRGB $ try hexColorLong <|> hexColorShort

hexColorShort :: Parser RGB
hexColorShort = do
    void $ char '#'
    digits <- count 3 hexDigit

    return $ unsafeParseHexIntoRGB $ concatMap (\x -> [x,x]) digits

hexColorLong :: Parser RGB
hexColorLong = do
    void $ char '#'
    unsafeParseHexIntoRGB <$> count 6 hexDigit
    -- TODO - force a space at the end

-- The string MUST be 6 character valid hex number. The only place to call
-- this function is from the hex parser.
unsafeParseHexIntoRGB :: String -> RGB
unsafeParseHexIntoRGB xs = RGB r g b
    where [r,g,b] = map unsafeParseSingle $ chunksOf 2 xs

parseSingle :: String -> Maybe Int
parseSingle input = case readHex input of
    [(n, "")] -> Just n
    _ -> Nothing

-- Only call this on a previously verified valid hex number.
unsafeParseSingle :: String -> Int
unsafeParseSingle input = fromMaybe undefined (parseSingle input)
