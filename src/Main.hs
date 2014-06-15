module Main where

import System.Environment
import Fass.Compiler

main :: IO ()
main = do
    as <- getArgs
    if null as then
      error "Usage: ./fass <file>"
    else
      compileFile (head as) >>= putStrLn
