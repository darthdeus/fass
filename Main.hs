module Main where

import Fass.Parser
import Fass.Printer
import Data.Either

main :: IO ()
main = do
    text <- readFile "sample.scss"
    print $ parseSCSS text

test :: IO ()
test = readFile "sample.scss" >>= (\c -> putStrLn . prettyPrint . flatten . head $ rights [parseSCSS c])
