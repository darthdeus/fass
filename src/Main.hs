module Main where

import Control.Monad ((>=>))
import System.Environment
import Fass.Compiler

main :: IO [()]
main = do
    as <- getArgs
    if null as || any (\a -> head a == '-') as then
      error "Usage: ./fass <file>"
    else
      mapM (compileFile >=> putStrLn) as
