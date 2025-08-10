module LeftRecursiveTwoPhaseExprParser
(expr,expr',texpr,tterm,tfactor,tnumber) where

import Mparseco
import Expr
import Debug.Trace

expr :: StringParser Expr
expr = MParser exprStringParser
    where
        exprStringParser s =
            do
                (ts,r) <- tokenize [] ["+","*"] s
                (e,r') <- parse texpr ts
                let s' = untokenize r'
                return $ (e,s')

expr' :: StringParser Expr
expr' = do
    ts <- tokenizer [] ["+","*"]
    e <- texpr ts
    return e

-- Perhaps using a monad transformer?

texpr :: TokensParser Expr
texpr =
    do
        a <- tterm
        texpr' a
    where
        texpr' a =
            do
                TOperator "+" <- oneToken
                b <- tterm
                texpr' $ Add a b
            <|>
            return a

tterm :: TokensParser Expr
tterm =
    do
        a <- tfactor
        tterm' a
    where
        tterm' a =
            do
                TOperator "*" <- oneToken
                b <- tfactor
                tterm' $ Mult a b
            <|>
            return a

tfactor :: TokensParser Expr
tfactor =
    tnumber
    <|>
    do
        lparToken'
        e <- texpr
        rparToken'
        return e

tnumber :: TokensParser Expr
tnumber = do
    -- TInt n <- intToken'
    TInt n <- oneToken
    return $ Num n
