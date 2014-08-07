{-# LANGUAGE OverloadedStrings #-}
module Fass.ParserSpec where

import Fass
import Fass.Types
import Fass.TestHelper

import Test.Hspec

spec :: Spec
spec = do
    describe "SCSS parser" $
        it "parses simple CSS" $
            parseSCSS "p { color: #fff; }" `matchRight` [Nested (Ruleset "p" [Rule "color" "#fff"])]
