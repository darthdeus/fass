module Main where

import Fass.Parser
import Fass.Compiler
import Fass.Types
import Fass.Printer

import Data.Either

main = do
    text <- readFile "sample.scss"
    print $ parseSCSS text
