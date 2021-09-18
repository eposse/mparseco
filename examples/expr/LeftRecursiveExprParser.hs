module LeftRecursiveExprParser (expr,term,factor,number) where

import Mparseco
import Expr

expr :: StringParser Expr
expr =
    do
        a <- term
        expr' a
    where
        expr' a =
            do
                literalChar '+'
                b <- term
                expr' $ Add a b
            <|>
            return a

term :: StringParser Expr
term =
    do
        a <- factor
        term' a
    where
        term' a =
            do
                literalChar '*'
                b <- factor
                term' $ Mult a b
            <|>
            return a

factor :: StringParser Expr
factor =
    number
    <|>
    do
        literalChar '('
        e <- expr
        literalChar ')'
        return e

number :: StringParser Expr
number = do
    n <- naturalNumber
    return $ Num n
