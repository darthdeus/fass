module Main where

import Fass.Compiler

main :: IO ()
main = do
    text <- readFile "sample.scss"
    compile text >>= print

test :: IO ()
test = readFile "sample.scss" >>= debugOutput >>= putStrLn

debugOutput :: String -> IO String
debugOutput c = compile c
