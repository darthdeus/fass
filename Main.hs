module Main where

import Fass.Parser

main :: IO ()
main = do
    text <- readFile "sample.scss"
    print $ parseSCSS text
