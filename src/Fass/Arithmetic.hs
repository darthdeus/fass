{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Fass.Arithmetic where

import Data.Monoid

data Value = RGBA Int Int Int Float -- ^ rgb can be represented as rgba(_,_,_,1.0)
           | Number Int             -- ^ and later optimized away when compiling
           | Literal String
           deriving Show

class Div a where
    divide :: a -> a -> a

instance Div Int where
    divide = div

instance Div Float where
    divide = (/)

data Expr a where
    Lit :: Value -> Expr Value
    Plus :: Expr a -> Expr a -> Expr a
    Minus :: Expr a -> Expr a -> Expr a
    Mult :: Expr a -> Expr a -> Expr a
    Div :: Expr a -> Expr a -> Expr a

deriving instance Show (Expr a)

runLit :: Expr Value -> Value
runLit (Lit x) = x

instance Monoid Value where
    mempty = Literal ""

    mappend (Number x) (Number y) = Number (x + y)
    mappend (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) =
        RGBA (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)
    mappend (Literal x) (Literal y) = Literal $ x ++ y
    mappend x y = Literal $ stringRep x ++ stringRep y

data Color = Color Int Int Int Float

op :: (forall a. (Div a, Num a) => a -> a -> a) -> Value -> Value -> Value
op f (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) = RGBA (r1 `f` r2) (g1 `f` g2) (b1 `f` b2) (a1 `f` a2) -- TODO add missing modulos
op f (Number x1) (Number x2) = Number (x1 `f` x2)


opPlus, opMinus, opMult, opDiv :: Value -> Value -> Value
opPlus  = op (+)
opMinus = op (-)
opMult  = op (*)
opDiv   = op divide

eval :: Expr a -> Expr Value
eval (Plus x y)  = Lit $ runEval x `opPlus` runEval y
eval (Minus x y) = Lit $ runEval x `opMinus` runEval y
eval (Mult x y)  = Lit $ runEval x `opMult` runEval y
eval (Div x y)   = Lit $ runEval x `opDiv` runEval y
eval (Lit x)     = Lit x

runEval :: Expr a -> Value
runEval = runLit . eval

-- This is not to be confused with a `Show` instance, as this is not for
-- debugging purposes, but rather for compiling the value down for concatenation
-- which can't be done in any other way, such as "#abc + hello".
stringRep :: Value -> String
stringRep (Literal s) = s
stringRep (Number x) = show x
stringRep (RGBA r g b a) = if a == 1.0
                           then "rgb(" ++ show r ++ "," ++ show g ++
                                "," ++ show b ++ ")"
                           else "rgba(" ++ show r ++ "," ++ show g ++
                                "," ++ show b ++ "," ++ show a ++ ")"
