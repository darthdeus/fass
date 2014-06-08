module Fass.EvaluatorSpec where

import Control.Monad.State
import Fass.Types
import Fass.Evaluator
import Test.Hspec

spec :: Spec
spec = do
    describe "inlineVariable" $ do
        it "does nothing for an empty ruleset" $ do
            let input = SASSRuleset "p" []
            flip evalState emptyEnv (inlineVariables input) `shouldBe` input
