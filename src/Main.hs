module Main where

import Fass.Compiler

main :: IO ()
main = do
    -- TODO - parse ARGV + print help
    text <- readFile "sample.scss"
    compile text >>= print

test :: IO ()
test = readFile "sample.scss" >>= debugOutput >>= putStrLn

debugOutput :: String -> IO String
debugOutput = compile
