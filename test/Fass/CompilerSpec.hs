module Fass.CompilerSpec where

import Data.List
import Fass.Compiler
import System.Directory
import System.FilePath.Posix
import Test.Hspec

spec :: Spec
spec = describe "Compiler" $ do
    it "works for a simple SCSS" $ do
        let input = "p { span { color: red; } }"
        let output = "p span {\ncolor: red;\n}\n\n"

        compile input `shouldBe` output

    it "passes the sass-spec tests" $ do
        root <- getCurrentDirectory
        let prefix = root </> "sass-spec" </> "spec" </> "basic"
        folders <- getDirectoryContents $ prefix

        let tests = folders \\ [".", ".."]

        mapM_ (\x -> runSpec $ prefix </> x) tests

runSpec :: FilePath -> IO ()
runSpec prefix = do
    input <- readFile $ prefix </> "input.scss"
    expectedOutput <- readFile $ prefix </> "expected_output.css"

    compile input `shouldBe` expectedOutput
