module Fass.Parser.ColorSpec where

import Fass.Parser.Color
import Test.Hspec
import Text.Parsec
import Text.Parsec.String

-- TODO - extract shared logic into something like TestHelper
testParser :: Parser a -> String -> Either ParseError a
testParser parser = parse parser "test parser"

matchRight :: (Show a, Show b, Eq b) => Either a b -> b -> IO ()
matchRight ex y = case ex of
    Left x -> fail $ show x
    Right x -> x `shouldBe` y

spec :: Spec
spec = do
    describe "hexColorShort" $ do
        it "works for a simple hex color" $ do
            testParser hexColorShort "#fff" `matchRight` "fff"
            testParser hexColorShort "#123" `matchRight` "123"

    describe "hexColorLong" $ do
        it "works for a simple hex color" $ do
            testParser hexColorLong "#ffffff" `matchRight` "ffffff"
            testParser hexColorLong "#fafafa" `matchRight` "fafafa"
