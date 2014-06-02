module Main where

import Test.Hspec
import Text.Parsec.Error
import Text.Parsec.Pos
import Fass

instance Eq ParseError where
    x == y = errorPos x == errorPos y &&
             errorMessages x == errorMessages y

foo :: Either Int String
foo = Left 3

main :: IO ()
main = hspec $
  describe "parser" $ do
    it "handles invalid data" $
      parseSCSS "invalid string" `shouldBe` Left (newErrorMessage (UnExpect "s") (newPos "SCSS Parser" 1 9))
