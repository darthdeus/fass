module Fass.Parser.Value where

import Fass.Parser.Color
import Fass.Parser.Helper

import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.String

data Units = Pixel
           | Em
           | Ex
           | Vh

data ValueTerm = Color RGBA
               | Size Float String
                deriving (Show, Eq)

term :: Parser ValueTerm
term = try (Color <$> colorParser) <|> size

-- Parser for property value where there is size required,
-- such as in "width: 100px" the parser would match on "100px"
size :: Parser ValueTerm
size = do
    n <- floatNumber
    s <- units

    return $ Size n s

-- Parser for CSS units, for example 10px, 2.3em, etc.
units :: Parser String
units = try (string "em") <|> try (string "ex") <|> string "px" <|> string "%"
