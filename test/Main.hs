module Main where

import Control.Monad
import Fass
import Fass.Parser
import Fass.Types
import Test.Hspec
import Test.Hspec.Formatters
import Test.Hspec.Runner
import Text.Parsec.Prim
import Text.Parsec.String

testParser parser input = parse parser "test parser" input

matchRight :: (Show a, Show b, Eq b) => Either a b -> b -> IO ()
matchRight ex y = case ex of
    Left x -> fail $ show x
    Right x -> x `shouldBe` y

testParserEqual parser input = testParser parser input `matchRight` input

-- let errorToString = either (Left . show) Right
-- let per = errorToString . parseSCSS

main :: IO ()
main = void $ hspecWith (defaultConfig { configFormatter = progress }) $ do
    describe "SCSS parser" $ do
        it "parses simple CSS" $
            parseSCSS "p { color: #fff; }" `matchRight`
                [SASSNestedRuleset (SASSRuleset "p" [SASSRule "color" "#fff"])]

    describe "rule parser" $ do
        it "works for plain CSS rules" $ do
            testParser rule "color: red" `matchRight` SASSRule "color" "red"
            testParser rule "color: red;" `matchRight` SASSRule "color" "red"

        it "works for rules with variables in them" $
            testParser rule "color: $header-bg" `matchRight` SASSRule "color" "$header-bg"

        it "works for functions" $
            testParser rule "color: rgba(255, 255, 0)" `matchRight` SASSRule "color" "rgba(255, 255, 0)"

    describe "variable parser" $ do
        it "works for integers" $
            testParser variable "$a: 3" `matchRight` SASSVariable "a" "3"

        it "works for colors" $ do
            testParser variable "$hello: #fff" `matchRight` SASSVariable "hello" "#fff"

        it "works for rgba colors" $ do
            testParser variable "$header-bg: rgba(255, 255, 0)" `matchRight`
                SASSVariable "header-bg" "rgba(255, 255, 0)"

    describe "selector parser" $ do
        it "works for element names" $ do
            testParserEqual selector "p"
            testParserEqual selector "span"
            testParserEqual selector "canvas"

        it "works for ids" $ do
            testParserEqual selector "#something"
            testParserEqual selector "#id-with-dashes"
            testParserEqual selector "#underscores_and-dashes"
            testParserEqual selector "#underscores_and-dashes123_4-5"

        it "works for class names" $ do
            testParserEqual selector ".important"

        it "classes and ids can be combined" $ do
            testParserEqual selector "#something.alert.alert-important"

        it "elements can be nested" $ do
            testParserEqual selector "p span"

        it "operators work as well" $ do
            testParserEqual selector "p > span"
            testParserEqual selector ".container > .div p > span"

        it "works with pseudo classes" $ do
            testParserEqual selector "p:hover"

        it "works for attributes" $ do
            testParserEqual selector "p[data-highlight]"
            testParserEqual selector "p[data-highlight=true]"

        it "complex combinations of everything else" $ do
            testParserEqual selector "body > #container a:hover"

    describe "property name parser" $ do
        it "works for simple strings" $ do
            testParserEqual propertyName "color"
            testParserEqual propertyName "background"

        it "works with dashes as well" $ do
            testParserEqual propertyName "background-color"

        it "even IE6 hacks should work" $ do
            testParserEqual propertyName "*color"

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
            testParser ruleset "p {}" `matchRight` SASSNestedRuleset (SASSRuleset "p" [])
            testParser ruleset ".red {}" `matchRight` SASSNestedRuleset (SASSRuleset ".red" [])
            testParser ruleset "  div   {}" `matchRight` SASSNestedRuleset (SASSRuleset "div" [])

        it "works for simple properties" $ do
            testParser ruleset "p { color: red; }" `matchRight`
                SASSNestedRuleset (SASSRuleset "p" [SASSRule "color" "red"])

            testParser ruleset "p { background-color: #fff; }" `matchRight`
                SASSNestedRuleset (SASSRuleset "p" [SASSRule "background-color" "#fff"])

        it "works for nested rulesets" $ do
            testParser ruleset "p { color: red; span { color: #f0f0fa; } }" `matchRight`
                SASSNestedRuleset (SASSRuleset "p"
                                   [SASSRule "color" "red",
                                    SASSNestedRuleset (SASSRuleset "span" [SASSRule "color" "#f0f0fa"])])
