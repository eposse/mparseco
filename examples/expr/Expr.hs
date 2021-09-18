module Expr (Expr(..)) where

data Expr =
    Num Int
  | Add Expr Expr
  | Mult Expr Expr
  deriving (Show, Eq)
