module Main where

import Fass.Compiler

main :: IO ()
main = do
    text <- readFile "sample.scss"
    print $ compile text

test :: IO ()
test = readFile "sample.scss" >>= return . debugOutput >>= putStrLn

debugOutput :: String -> String
debugOutput c = compile c
