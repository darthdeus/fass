module Main where

import Fass.Parser

main = do
    text <- readFile "sample.scss"
    print $ parseSCSS text
