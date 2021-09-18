module FullyParenthesisedExprParser (expr,number) where

import Mparseco
import Expr

expr :: StringParser Expr
expr = number <|> addition <|> multiplication

number :: StringParser Expr
number = do
    n <- naturalNumber
    return $ Num n

addition :: StringParser Expr
addition = do
    literalChar '('
    l <- expr
    literalChar '+'
    r <- expr
    literalChar ')'
    return $ Add l r

multiplication :: StringParser Expr
multiplication = do
    literalChar '('
    l <- expr
    literalChar '*'
    r <- expr
    literalChar ')'
    return $ Mult l r
