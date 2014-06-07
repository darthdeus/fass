module Main where

import Fass
import Fass.Types
import Test.Hspec
import Fass.Parser
import Text.Parsec.Prim

testParser parser input = parse parser "test parser" input

matchRight :: (Show a, Show b, Eq b) => Either a b -> b -> IO ()
matchRight ex y = case ex of
    Left x -> fail $ show x
    Right x -> x `shouldBe` y

testParserEqual parser input = testParser parser input `matchRight` input

-- let errorToString = either (Left . show) Right
-- let per = errorToString . parseSCSS

main :: IO ()
main = hspec $ do
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
