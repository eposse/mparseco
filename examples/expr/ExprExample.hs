module Main where

import Debug.Trace
import Mparseco
import Expr
import FullyParenthesisedExprParser as Fpep
import LeftRecursiveExprParser as Lrep
import LeftRecursiveExprTokParser as Lretp

r1 = parse Fpep.number "123"

r2 = parse Fpep.number "1.23"

r3 = parse Fpep.expr "(6174+3825)"

r4 = parse Fpep.expr "(6174*3825)"

r5 = parse Fpep.expr "(1+(2*3))"

r6 = parse Fpep.expr "((1+2)*3)"

r7 = parse Lrep.expr "1+2+3"

r8 = parse Lrep.expr "1+(2+3)"

r9 = parse Lrep.expr "(1+2)+3"

r10 = parse Lrep.expr "1+2*3"

r11 = parse Lrep.expr "1+(2*3)"

r12 = parse Lrep.expr "(1+2)*3"

r13 = parse Lrep.expr "1*2+3"

r14 = parse Lrep.expr "1*(2+3)"

r15 = parse Lrep.expr "(1*2)+3"

r16 = parse Lrep.expr "1 + 2 * 3"

r17 = parse Lretp.number "2"

r18 = parse Lretp.expr "1+2*3"

r19 = parse Lretp.expr "1+(2*3)"

r20 = parse Lretp.expr "(1+2)*3"

r21 = parse Lretp.expr "1*2+3"

r22 = parse Lretp.expr "1*(2+3)"

r23 = parse Lretp.expr "(1*2)+3"

main = do
    putStrLn "Expression parsing example"
