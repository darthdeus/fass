module Fass.Parser.ColorSpec where

import Fass.Parser.Color
import Fass.TestHelper

import Test.Hspec

-- TODO - extract shared logic into something like TestHelper
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
