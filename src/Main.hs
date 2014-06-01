module Main where

import Fass
import Data.Either

main :: IO ()
main = do
    text <- readFile "sample.scss"
    print $ parseSCSS text

test :: IO ()
test = readFile "sample.scss" >>= (\c -> putStrLn . prettyPrint . head $ rights [parseSCSS c])
