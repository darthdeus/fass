{-# LANGUAGE OverloadedStrings #-}
module Fass.ParserSpec where

import Control.Monad
import Fass
import Fass.Parser
import Fass.Types
import Fass.TestHelper

import Test.Hspec
import Test.Hspec.Formatters
import Test.Hspec.Runner

testSelector :: String -> IO ()
testSelector string = testParser selector string `matchRight` Selector string

main :: IO ()
main = void $ hspecWith (defaultConfig { configFormatter = progress }) spec

spec :: Spec
spec = do
    describe "SCSS parser" $
        it "parses simple CSS" $
            parseSCSS "p { color: #fff; }" `matchRight`
                [Nested (Ruleset "p" [Rule "color" "#fff"])]

    describe "rule parser" $ do
        it "works for plain CSS rules" $ do
            testParser rule "color: red" `matchRight` Rule "color" "red"
            testParser rule "color: red;" `matchRight` Rule "color" "red"

        it "allows numbers in rule names" $
            testParser rule "p01: red;" `matchRight` Rule "p01" "red"

        it "works for rules with variables in them" $
            testParser rule "color: $header-bg" `matchRight` Rule "color" "$header-bg"

        it "works for functions" $
            testParser rule "color: rgba(255, 255, 0)" `matchRight` Rule "color" "rgba(255, 255, 0)"

        it "works for hexadecimal numbers" $
            testParser rule "color: #fafafa" `matchRight` Rule "color" "#fafafa"

        it "works for basic arithmetic" $ do
            testParser rule "color: #fafafa + hello" `matchRight` Rule "color" "#fafafa + hello"
            testParser rule "color: #fafafa + #bbb" `matchRight` Rule "color" "#fafafa + #bbb"

    describe "variable parser" $ do
        it "works for integers" $
            testParser variable "$a: 3" `matchRight` Variable "a" "3"

        it "works for colors" $ do
            testParser variable "$red: #fff" `matchRight` Variable "red" "#fff"
            testParser variable "$blue: #ff0f0c" `matchRight` Variable "blue" "#ff0f0c"

        it "works for rgb and rgba colors" $ do
            testParser variable "$header-bg: rgba(255, 255, 0)" `matchRight`
                Variable "header-bg" "rgba(255, 255, 0)"

            testParser variable "$header-bg: rgb(255, 3, 0)" `matchRight`
                Variable "header-bg" "rgb(255, 3, 0)"


    describe "selector parser" $ do
        it "works for element names" $ do
            testSelector "p"
            testSelector "span"
            testSelector "canvas"

        it "works for ids" $ do
            testSelector "#something"
            testSelector "#id-with-dashes"
            testSelector "#underscores_and-dashes"
            testSelector "#underscores_and-dashes123_4-5"

        it "works for class names" $ do
            testSelector ".important"
            testSelector ".alert.alert-important"

        it "classes and ids can be combined" $ do
            testSelector "#something.alert.alert-important"
            testSelector "div#something.alert.alert-important"

        it "elements can be nested" $ do
            testSelector "p span"
            testSelector "html body span"

        it "operators work as well" $ do
            testSelector "p > span"
            testSelector ".container > .div p > span"

        it "works with pseudo classes" $ do
            testSelector "p:hover"
            testSelector "p.red:hover"

        it "works for attributes" $ do
            testSelector "p[data-highlight]"
            testSelector "p[data-highlight=true]"

        it "complex combinations of everything else" $ do
            testSelector "body > #container a:hover"
            testSelector "body > div#container[data-red] a:hover"

        it "works with selector groups" $ do
            testSelector "span, strong, em"

    describe "property name parser" $ do
        it "works for simple strings" $ do
            testParserEqual propertyName "color"
            testParserEqual propertyName "background"

        it "works with dashes as well" $ do
            testParserEqual propertyName "background-color"
            testParserEqual propertyName "font-weight"

        it "even IE6 hacks should work" $ do
            testParserEqual propertyName "*color"
            testParserEqual propertyName "*_color"

    describe "property value parser" $ do
        it "works for integers" $ do
            testParserEqual propertyValue "0"
            testParserEqual propertyValue "10"

        it "works for values with units" $ do
            testParserEqual propertyValue "0px"
            testParserEqual propertyValue "10em"
            testParserEqual propertyValue "10ex"
            testParserEqual propertyValue "10vh"

        it "works for variable names" $ do
            testParserEqual propertyValue "$header"
            testParserEqual propertyValue "$header-bg"
            testParserEqual propertyValue "$header_bg"

    describe "ruleset parser" $ do
        it "works for empty ruleset" $ do
            testParser ruleset "p {}" `matchRight` Nested (Ruleset "p" [])
            testParser ruleset ".red {}" `matchRight` Nested (Ruleset ".red" [])
            testParser ruleset "  div   {}" `matchRight` Nested (Ruleset "div" [])

        it "works for simple properties" $ do
            testParser ruleset "p { color: red; }" `matchRight`
                Nested (Ruleset "p" [Rule "color" "red"])

            testParser ruleset "p { background-color: #fff; }" `matchRight`
                Nested (Ruleset "p" [Rule "background-color" "#fff"])

        it "works for nested rulesets" $
            testParser ruleset "p { color: red; span { color: #f0f0fa; } }" `matchRight`
                Nested (Ruleset "p"
                        [Rule "color" "red",
                         Nested (Ruleset "span" [Rule "color" "#f0f0fa"])])

        it "parses imports inside rulests" $ do
            testParser ruleset "p { @import \"example.scss\"; }" `matchRight`
                Nested (Ruleset "p" [Import "example.scss"])

    describe "entityList parser" $ do
        it "works for simple nested elements" $ do
            testParser entityList "div {\n  img {\n    border: 0px;\n  }\n}" `matchRight`
                [Nested (Ruleset "div"
                        [Nested (Ruleset "img" [Rule "border" "0px"])])]

        it "works for simple variables" $ do
            testParser entityList "$color: red;\na {\ncolor: $color;\n}" `matchRight`
                [Variable "color" "red",
                 Nested (Ruleset "a" [Rule "color" "$color"])]

        it "works for top level comments" $ do
            testParser entityList "/* hello world */\np { color: red; }" `matchRight`
                [Comment " hello world ", Nested (Ruleset "p" [Rule "color" "red"])]

        it "works for grouped selectors" $ do
            testParser entityList "h1, h2 { font-weight: bold; }" `matchRight`
                [Nested (Ruleset "h1, h2" [Rule "font-weight" "bold"])]

    describe "comment parser" $ do
        it "works for empty comments" $ do
            testParser comment "/**/" `matchRight` Comment ""
            testParser comment "/* */" `matchRight` Comment " "
            testParser comment "/*\t*/" `matchRight` Comment "\t"
            testParser comment "/*\n*/" `matchRight` Comment "\n"

        it "works for comments with content" $ do
            testParser comment "/* hello world */" `matchRight` Comment " hello world "

        it "swallows single line comments" $ do
            testParser comment "// foo" `matchRight` Null


    describe "import parser" $ do
        it "parses simple imports" $ do
            testParser importParser "@import \"foo.scss\"" `matchRight` Import "foo.scss"
            testParser importParser "@import \"foo.scss\";" `matchRight` Import "foo.scss"
