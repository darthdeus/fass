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
    describe "colorParser" $ do
        it "works for both short and long hex colors" $ do
            testParser colorParser "#ffffff" `matchRight` RGBA 255 255 255 0.0
            testParser colorParser "#fff" `matchRight` RGBA 255 255 255 0.0

        it "works for rgb" $ do
            testParser colorParser "rgb(255, 255, 0)" `matchRight` RGBA 255 255 0 0.0

        it "works for rgba" $ do
            testParser colorParser "rgba(255, 255, 0, 3.14)" `matchRight` RGBA 255 255 0 3.14
