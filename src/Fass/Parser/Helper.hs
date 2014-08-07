module Fass.Parser.Helper where

import Control.Monad
import Text.Parsec
import Text.Parsec.String

paddedChar :: Char -> Parser ()
paddedChar c = void $ spaces >> char c >> spaces
