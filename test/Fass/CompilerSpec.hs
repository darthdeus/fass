module Fass.CompilerSpec where

import Data.List
import Fass.Compiler
import System.Directory
import System.FilePath.Posix
import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = describe "Compiler" $ do
    it "passes the sass-spec tests" $ do
        root <- getCurrentDirectory
        let prefix = root </> "sass-spec"
        folders <- getDirectoryContents $ prefix

        let tests = folders \\ [".", ".."]

        mapM_ (\x -> runSpec $ prefix </> x) tests

    it "works for simple variables" $ do
        let input = "$color: red;\na {\ncolor: $color;\n}"

        compile input `shouldBe` "a {\n  color: red; }\n"

runSpec :: FilePath -> IO ()
runSpec prefix = do
    input <- readFile $ prefix </> "input.scss"
    expectedOutput <- readFile $ prefix </> "expected_output.css"

    putStrLn prefix

    -- TODO - Figure out what exactly are the rules for trailing \n.
    -- It seems that for single ruleset there is no trailing \n,
    -- but there is one when there is more than one ruleset. I'm not 100%
    -- sure about this, which is why I'll be trimming them here in both cases.

    let left = trim (compile input)
    let right = (trim expectedOutput)

    -- putStrLn $ left ++ "\n\n"
    minify left `shouldBe` minify right

-- TODO - figure out a more effective way to do this
-- trim :: string -> string
-- trim = T.unpack . T.strip . T.pack
