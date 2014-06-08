module Fass.EvaluatorSpec where

import qualified Data.Map as M
import Control.Monad.State
import Fass.Types
import Fass.Evaluator
import Test.Hspec

eval :: (a -> State SASSEnv b) -> a -> b
eval f = flip evalState emptyEnv . f

spec :: Spec
spec = do
    describe "inlineVariables" $ do
        it "does nothing for an empty ruleset" $ do
            let input = SASSRuleset "p" []
            eval inlineVariables input `shouldBe` input

    describe "inlineEntity" $ do
        it "replaces variable entities with nothing" $ do
            let definition = SASSVariable "header" "#fff"

            let (ent, env) = runState (inlineEntity definition) emptyEnv
            ent `shouldBe` SASSNothing
            M.lookup "header" env `shouldBe` Just "#fff"
            M.size env `shouldBe` 1

        it "doesn't change rules with no variables" $ do
            let entity = SASSRule "color" "red"
            eval inlineEntity entity `shouldBe` entity

        it "replaces variable usages with their actual value" $ do
            let initState = M.fromList [("header", "#fafafa")]
            let entity = inlineEntity (SASSRule "color" "$header")

            evalState entity initState `shouldBe`
                SASSRule "color" "#fafafa"
