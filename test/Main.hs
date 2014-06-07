module Main where

import Data.Either
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

        -- let errorToString = either (Left . show) Right
        -- let per = errorToString . parseSCSS

main :: IO ()
main = hspec $ do
    -- describe "variable parser" $ do
    --     it "must be prefixed by a $" $ do
    --         testParser variable "variable parser" `shouldBe` Left _

  describe "parser" $ do
    it "handles invalid data" $ do
      parseSCSS "invalid string" `shouldSatisfy` isLeft

    it "parses simple CSS" $
      case parseSCSS "p { color: #fff; }" of
          Right x -> x `shouldBe` [SASSNestedRuleset (SASSRuleset "p" [SASSRule "color" "#fff"])]
          Left x -> fail $ show x

    describe "variable parser" $ do
        it "works for integers" $
            testParser variable "$a: 3" `matchRight` SASSVariable "a" "3"

        it "works for colors" $ do
            testParser variable "$hello: #fff" `matchRight` SASSVariable "hello" "#fff"

        it "works for rgba colors" $ do
            testParser variable "$header-bg: rgba(255, 255, 0)" `matchRight`
                SASSVariable "header-bg" "rgba(255, 255, 0)"
