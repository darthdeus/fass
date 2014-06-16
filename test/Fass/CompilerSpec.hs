module Fass.CompilerSpec where

import Data.List
import Fass.Compiler
import Fass.Parsers.SCSS
import System.Directory
import System.FilePath.Posix
import Test.Hspec

spec :: Spec
spec = describe "Compiler" $ do
    it "passes the sass-spec tests" $ do
        root <- getCurrentDirectory
        let prefix = root </> "sass-spec" </> "spec" </> "basic"
        folders <- getDirectoryContents $ prefix

        let tests = folders \\ [".", ".."]

        mapM_ (\x -> runSpec $ prefix </> x) tests

    it "works for simple variables" $ do
        let input = "$color: red;\na {\ncolor: $color;\n}"

        compile (parseSCSS input) `shouldBe` "a {\n  color: red; }"

runSpec :: FilePath -> IO ()
runSpec prefix = do
    input <- compileFile $ prefix </> "input.scss"
    expectedOutput <- readFile $ prefix </> "expected_output.css"

    putStrLn prefix
    input `shouldBe` expectedOutput
