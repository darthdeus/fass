module Main where

import Test.Hspec
import Text.Parsec.Error
import Text.Parsec.Pos
import Fass
import Fass.Types

instance Eq ParseError where
  x == y = errorPos x == errorPos y &&
   errorMessages x == errorMessages y

foo :: Either Int String
foo = Left 3

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "handles invalid data" $ do
      let (Left err) = parseSCSS "invalid string"
      (sourceLine . errorPos $ err) `shouldBe` 1
      (sourceColumn . errorPos $ err) `shouldBe` 9

    it "parses simple CSS" $
      parseSCSS "p { color: #fff; }" `shouldBe` Right [SASSNestedRuleset (SASSRuleset "p" [SASSRule "color" "#fff"])]

    it "parses variables" $
      parseSCSS "$a: 3" `shouldBe` Right [SASSVariable "a" "3"]
