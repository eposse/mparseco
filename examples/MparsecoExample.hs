module Main where

import Mparseco

r1 = parse oneChar "qwerty"

r2 = parse naturalNumbers "742"

p3 = do
    n <- naturalNumbers
    return $ Just (n + 1)

r3 = parse p3 "742"

data Apparatus =
      Gizmo Int String
    | Widget Int Int
    | Contraption Apparatus String
    deriving Show

r4 = do
    (a,y) <- parse naturalNumbers "142"
    return $ Gizmo a y

p5 n = do
    m <- naturalNumber
    return $ Widget n m

r5 = do
    (a,y) <- parse naturalNumbers "142"
    (b,z) <- parse (p5 a) y
    return $ Contraption b z

r6 = parse (naturalNumbers >>= p5) "143"

r7 = parse identifiers "a_1"

r8 = parse (naturalNumberLiteral <|> identifier) "42xy"

r9 = parse (naturalNumberLiteral <|> identifier) "xy42"

r10 = parse (naturalNumberLiteral <|> identifier) "xy.42"

r11 = parse (naturalNumberLiterals <|> identifiers) "42xy"

r12 = parse (naturalNumberLiterals <|> identifiers) "xy42"

r13 = parse (naturalNumberLiterals <|> identifiers) "xy.42"

r14 = parse (literal "qwe") "qwerty"

r15 = parse boolToken "true"

r16 = parse boolToken "false"

r17 = parse boolToken "True"

r18 = parse boolToken "truee"

r19 = parse charToken "'a'"

r20 = parse charToken "a"

r21 = parse charToken "'bc'"

r22 = parse charToken "'d'e"

r23 = parse intToken "42"

r24 = parse intToken "42.36"

r25 = parse intToken " 42"

r26 = parse intToken "-53"

r27 = parse intToken "+64"

r28 = parse (next boolToken) " false"

r29 = parse (next intToken) " \t\n78-3"

r30 = parse stringToken "\"qwerty\""

r31 = parse (next stringToken) "  \"qwerty\"zxc"

r32 = parse (next stringToken) "  \"qwerty\"\"zxc"

r33 = parse (next stringToken) "\"qwerty\"zxc"

r34 = basicTokenize ""

r35 = basicTokenize "1"

r36 = basicTokenize "1a"

r37 = parse basicToken ""

r38 = parse basicToken "true"

r39 = parse basicToken "false"

r40 = parse basicToken "1"

r41 = parse basicToken "-1"

r42 = parse basicToken "'z'"

r43 = parse basicToken "\"abc\""

r44 = parse basicToken "var"

r45 = parse (token ["var","fal","null"] []) ""

r46 = parse (token ["var","fal","null"] []) "true"

r47 = parse (token ["var","fal","null"] []) "false"

r48 = parse (token ["var","fal","null"] []) "1"

r49 = parse (token ["var","fal","null"] []) "-1"

r50 = parse (token ["var","fal","null"] []) "'z'"

r51 = parse (token ["var","fal","null"] []) "\"abc\""

r52 = parse (token ["var","fal","null"] []) "var"

r53 = parse (token ["var","fal","null"] []) "variable"

r54 = parse (token ["var","fal","null"] []) "fal"

r55 = parse (token ["var","fal","null"] []) "null"

r56 = tokenize ["var","oof"] ["="] "var iable = oofs 42"

r57 = tokenize ["var","oof"] ["="] "variable = oofs 42"

r58 = tokenize ["var","oof"] ["="] "variable = oof 42"

r59 = tokenize ["var","oof"] ["="] "var iable = oof 42"

r60 = nonDetTokenize ["var","oof"] ["="] "var iable = oofs 42"

r61 = nonDetTokenize ["var","oof"] ["="] "variable = oofs 42"

r62 = nonDetTokenize ["var","oof"] ["="] "variable = oof 42"

r63 = nonDetTokenize ["var","oof"] ["="] "var iable = oof 42"


main = do
    putStrLn "Hello Mparseco"
    putStrLn $ "parse r1 = " ++ show r1
    putStrLn $ "parse r2 = " ++ show r2
