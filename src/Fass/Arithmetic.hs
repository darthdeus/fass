{-# LANGUAGE GADTs #-}
module Fass.Arithmetic where

data Value = Hex Int
           | RGBA Int Int Int Float -- ^ rgb can be represented as rgba(_,_,_,1.0)
           | Number Int             -- ^ and later optimized away when compiling
           | Literal String
           deriving Show

data Expr = Lit Value
          | OpPlus Expr Expr
          | OpMinus Expr Expr
          | OpMult Expr Expr
          | OpDiv Expr Expr
          deriving Show

-- eval :: Term -> Term
