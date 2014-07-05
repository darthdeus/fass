{-# LANGUAGE GADTs #-}
module Fass.Arithmetic where

import Data.Monoid
import Data.List.Split
import Numeric

data Value = Hex Int
           | RGBA Int Int Int Float -- ^ rgb can be represented as rgba(_,_,_,1.0)
           | Number Int             -- ^ and later optimized away when compiling
           | Literal String
           deriving Show

-- This is not to be confused with a `Show` instance, as this is not for
-- debugging purposes, but rather for compiling the value down for concatenation
-- which can't be done in any other way, such as "#abc + hello".
stringRep :: Value -> String
stringRep (Hex x) = show x -- TODO - convert this to actual hex int
stringRep (Literal s) = s
stringRep (Number x) = show x
stringRep (RGBA r g b a) = if a == 1.0
                           then "rgb(" ++ show r ++ "," ++ show g ++
                                "," ++ show b ++ ")"
                           else "rgba(" ++ show r ++ "," ++ show g ++
                                "," ++ show b ++ "," ++ show a ++ ")"

instance Monoid Value where
    mempty = Literal ""

    mappend (Hex x) (Hex y) = Hex (x + y)
    mappend (Number x) (Number y) = Number (x + y)
    mappend (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) =
        RGBA (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)
    mappend x y = Literal $ stringRep x ++ stringRep y
                -- TODO - handle the missing cases

data Expr = Lit Value
          | OpPlus Expr Expr
          | OpMinus Expr Expr
          | OpMult Expr Expr
          | OpDiv Expr Expr
          deriving Show

data Color = Color Int Int Int Float

-- ^ Returns either a parsed color, or a string in case the parsing failed
parseHex :: String -> Either String Color
parseHex ('#':xs) =
    case length xs of
        -- 3 -> go . chunksOf 2 $ concatMap (\x -> [x,x]) xs
        6 -> undefined -- parseHexTerms $ chunksOf 2 xs
        otherwise -> Left xs


parseHexTerms :: (String, String, String, String) -> Maybe (Int, Int, Int, Int)
parseHexTerms (rs, gs, bs, as) = do
    r <- parseSingle rs
    g <- parseSingle gs
    b <- parseSingle bs
    a <- parseSingle as

    return (r, g, b, a)

parseSingle :: String -> Maybe Int
parseSingle input = case readHex input of
    [(n, "")] -> Just n
    _ -> Nothing

eval :: Expr -> Expr
eval (OpPlus (Lit a) (Lit b)) = Lit $ a <> b
