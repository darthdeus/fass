module Fass.TestHelper where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

testParser :: Parser a -> String -> Either ParseError a
testParser parser = parse parser "test parser"

matchRight :: (Show a, Show b, Eq b) => Either a b -> b -> IO ()
matchRight ex y = case ex of
    Left x -> fail $ show x
    Right x -> x `shouldBe` y


testParserEqual :: Parser String -> String -> IO ()
testParserEqual parser input = testParser parser input `matchRight` input
