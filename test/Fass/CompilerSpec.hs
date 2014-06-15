module Fass.CompilerSpec where

import Fass.Compiler
import Test.Hspec

spec :: Spec
spec = describe "Compiler" $ do
    it "works for a simple SCSS" $ do
        let input = "p { span { color: red; } }"
        let output = "p span {\ncolor: red;\n}\n\n"

        compile input `shouldBe` output
