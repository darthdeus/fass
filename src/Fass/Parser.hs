module Fass.Parser where

import Fass.Types
import Fass.Parser.Entity

import Text.Parsec

parseSCSS :: String -> Either ParseError [Entity]
parseSCSS = parse entityList "SCSS Parser"
