{-# LANGUAGE OverloadedStrings #-}
module Fass.CompilerSpec where

import Data.List
import Fass.Compiler
import Fass.Types
import System.Directory
import System.FilePath.Posix
import System.IO
import Test.Hspec

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

        result <- compile input
        result `shouldBe` "a {\n  color: red; }\n"

    describe "inlineImportWithFile" $ do
        it "is a noop for non-import entities" $ do
            let input = Rule "foo" "bar"
            inlineImportWithFile input >>= shouldBe [input]

        it "impots a given file" $ do
            let tmp = "/tmp/fass-test.scss"
            withFile tmp WriteMode (\h -> hPutStrLn h "p { color: red; }")

            result <- inlineImportWithFile (Import tmp)

            -- in case the test fails, remove teh file first
            removeFile tmp

            result `shouldBe` [Nested (Ruleset "p" [Rule "color" "red"])]

        it "resolves imports recursively" $ do
            let tmp1 = "/tmp/fass-test1.scss"
            let tmp2 = "/tmp/fass-test2.scss"

            withFile tmp1 WriteMode
                (\h -> hPutStrLn h "@import \"/tmp/fass-test2.scss\";")

            withFile tmp2 WriteMode
                (\h -> hPutStrLn h "p { color: red; }")

            result <- inlineImportWithFile (Import tmp1)

            removeFile tmp1
            removeFile tmp2

            result `shouldBe` [Nested (Ruleset "p" [Rule "color" "red"])]

runSpec :: FilePath -> IO ()
runSpec prefix = do
    input <- readFile $ prefix </> "input.scss"
    expectedOutput <- readFile $ prefix </> "expected_output.css"

    putStrLn prefix

    -- TODO - Figure out what exactly are the rules for trailing \n.
    -- It seems that for single ruleset there is no trailing \n,
    -- but there is one when there is more than one ruleset. I'm not 100%
    -- sure about this, which is why I'll be trimming them here in both cases.

    left <- fmap trim (compile input)
    let right = (trim expectedOutput)

    -- putStrLn $ left ++ "\n\n"
    minify left `shouldBe` minify right

-- TODO - figure out a more effective way to do this
-- trim :: string -> string
-- trim = T.unpack . T.strip . T.pack
