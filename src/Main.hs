module Main where

import Fass
import Data.Either

main :: IO ()
main = do
    text <- readFile "sample.scss"
    print $ parseSCSS text

test :: IO ()
test = readFile "sample.scss" >>= return . debugOutput >>= putStrLn

debugOutput :: String -> String
debugOutput c = prettyPrint . head $ rights [parseSCSS c]
