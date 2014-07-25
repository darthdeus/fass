module Fass.Printer where

import Control.Lens
import Data.List
import Fass.Types

prettyPrint :: [Entity] -> String
prettyPrint entities = intercalate "\n" $ toListOf (traverse._Just) $ map (printEntity 0) entities

printEntity :: Int -> Entity -> Maybe String
printEntity i (Rule key value) = padded i $ key ++ ": " ++ value ++ ";"
printEntity i (Comment content) = padded i $ "/*" ++ content ++ "*/"
printEntity i (Nested (Ruleset (Selector s) rules)) = padded i $ s ++ " {\n" ++ printedRules rules ++ " }\n"
  where
    printedRules :: [Entity] -> String
    printedRules xs = intercalate "\n" $ toListOf (traverse._Just) $ map (printEntity (i + 2)) xs
printEntity _ _ = fail "Invalid input"

padded :: Int -> String -> Maybe String
padded i s = Just $ replicate i ' ' ++ s
