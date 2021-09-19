module LeftRecursiveExprTokParser
(expr,term,factor,number) where

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
                operatorToken "+"
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
                operatorToken "*"
                b <- factor
                term' $ Mult a b
            <|>
            return a

factor :: StringParser Expr
factor =
    number
    <|>
    do
        lparToken
        e <- expr
        rparToken
        return e

number :: StringParser Expr
number = do
    TInt n <- intToken
    return $ Num n

-- The following definition is incorrect:
--
--    number :: TokensParser Expr
--    number = do
--        TInt n <- intToken
--        return $ Num n
--
-- This is (roughly) equivalent to
--    number = intToken >>= \(TInt n) -> return $ Num n
-- We have that
--    (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- so if
--    number :: TokenParser Expr
-- then
--    m is TokenParser and b is Expr
-- so we would have
--    intToken :: TokenParser a
-- and
--    TInt n :: a
-- which means that
--   a is Token
-- hence
--   intToken :: TokenParser Token
-- which is a contradiction, because
--   intToken :: StringParser Token
