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
    describe "parseColor" $ do
        it "works for both short and long hex colors" $ do
            testParser hexColor "#ffffff" `matchRight` RGB 255 255 255
            testParser hexColor "#fff" `matchRight` RGB 255 255 255
