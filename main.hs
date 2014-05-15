module Main where

import Fass.Parser
import Fass.Compiler
import Fass.Types

main = do
    text <- readFile "sample.scss"
    print $ parseSCSS text
