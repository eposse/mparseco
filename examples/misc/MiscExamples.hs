module Main where

import Debug.Trace
import Mparseco
import MparsecoTests.Generators
import Test.QuickCheck as QC
import Data.List
import Data.Ord
import Data.Char

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

r64 = untokenize tokens where tokens = [ TKeyword "R" ]

r65 = tokenize ["R"] [] "R"

r66 = tokenize ["R"] [] "R "

r67 = untokenize tokens
    where
        tokens = [TString "&",TIdentifier "__K",TInt 5,TBool False,TChar '\719766']

r68 = tokenize [] [] r67

r69 = untokenize [TChar '\ETB']

r70 = tokenize [] [] r69

r71 = untokenize [TInt 0,TChar 'w',TString "f#",TString "\DC2c`",TChar 'g']

r72 = tokenize [] [] r71

r73 = untokenize [TString "1",TIdentifier "R",TString "\\"]

r74 = tokenize [] [] r73

r75 = untokenize [TChar '2',TIdentifier "_H32_M2G_",TKeyword "Gnf",TLPar,TChar '\t',TIdentifier "p",TOperator "=,<",TIdentifier "_sF_j_150"]

r76 = tokenize ["Gnf"] ["=,<"] r75

r77 = untokenize [TString "\\a\\a",TLPar]

r78 = tokenize [] [] r77

r79 = untokenize [TString "",TRPar,TString "* ",TLPar,TInt 0]

r80 = tokenize [] [] r79

r81t = [TOperator "#:|=",TString "-;66x\\\" ",TInt (-12),TBool False]
r81 = untokenize r81t
r81o = map operatorTokenString $ filter isOperatorToken r81t

r82 = tokenize [] r81o r81

r83t = [TChar '`',TChar '~',TRPar,TChar '\n',TLPar,TLPar,TIdentifier "__wCM7P0_",TLPar,TOperator "~+/$",TOperator ">/",TChar '\r',TLPar,TKeyword "dGFRYKyuyKjYmeFboanF",TIdentifier "neQs5K2_Hj__p",TKeyword "dDEMyyWBCw",TOperator "-",TBool True,TChar '\n',TInt 28,TLPar,TChar '\t',TInt 21,TLPar,TInt (-14),TString "\n\tQ+TA*\n\nF X\n \n,",TInt (-23),TInt 2,TRPar,TInt 29,TKeyword "bL",TBool True,TInt (-8)]
r83k = map keywordTokenString $ filter isKeywordToken r83t
r83o = map operatorTokenString $ filter isOperatorToken r83t
r83 = untokenize r83t

r84 = tokenize r83k r83o r83

r84t = fst . head $ r84

r84z = zip r83t r84t
r84d = [(t,t') | (t,t') <- r84z, t /= t']

-- TODO: tokenize negative numbers

r85t = [ TInt (-2) ]
r85 = untokenize r85t

r86 = tokenize [] [] r85

r87t = [ TOperator "-", TInt (-2) ]
r87o = ["-"]
r87 = untokenize r87t

r88 = tokenize [] r87o r87
r88' = tokenize [] r87o "--2"
r88'' = tokenize [] [] "--2"

r89 = tokenize [] [] "1-2"
r90 = tokenize [] ["-"] "1-2"
r91 = tokenize [] [] "1 -2"
r92 = tokenize [] ["-"] "1 -2"
r93 = tokenize [] [] "1- 2"
r94 = tokenize [] ["-"] "1- 2"
r95 = tokenize [] [] "1(-2)"
r96  = tokenize [] ["-"] "1(-2)"

r97t = [TInt 20,TIdentifier "_Ow",TOperator "=",TChar '*',TIdentifier "__J999Xk_Rj7N_1v_",TLPar,TRPar,TRPar,TOperator ">,!",TBool False,TInt 8,TLPar,TInt (-5),TOperator "+#,-",TBool True,TOperator "=?<",TRPar,TString "06c\\r\\r9;",TBool True,TRPar,TRPar,TOperator "!/|;",TIdentifier "C3sy_",TInt (-8)]
r97k = map keywordTokenString $ filter isKeywordToken r97t
r97o = map operatorTokenString $ filter isOperatorToken r97t
r97 = untokenize r97t

r98 = tokenize r97k r97o r97
r98t = fst . head $ r98
r98z = zip r97t r98t

r99 = tokenize [] ["=","=?"] "1=?2"

r100t = [TBool False,TOperator ".",TKeyword "xYbWqsZZ",TIdentifier "_y_80O",TRPar,TIdentifier "_e",TLPar,TLPar,TInt 5,TString "<",TIdentifier "l__H_1_",TChar '\n',TChar 'S',TOperator "_",TRPar,TLPar,TChar '^',TInt (-24),TString "0<\\r8-?\\r/9^_\\tM*\\t",TRPar,TChar '<']
r100k = map keywordTokenString $ filter isKeywordToken r100t
r100o = map operatorTokenString $ filter isOperatorToken r100t
r100 = untokenize r100t

r101 = tokenize r100k r100o r100
r101t = fst . head $ r101
r101z = zip r100t r101t

r102 = tokenize [] [] "_x"
r103 = tokenize [] ["_"] "_x"
r104 = tokenize [] [] "x_y"
r105 = tokenize [] ["_"] "x_y"

-- r106t = [TOperator "=.?;",TLPar,TKeyword"ysSURgFtK",TRPar,TOperator ".:#>",TOperator ",",TKeyword "bVLkbsDOrBDanJICrVSYFsqFIkPcWZpnrrDUaFVTcMwToMEuhyR",TLPar,TString "\\n/\\t\\t&\\n0>\\n",TLPar,TString "\\t\\n7<J9u3?/;\\t\\n\\r9\\t|\\t*.\\t8I",TIdentifier "W_08t_4__1_Hq1L____O9_",TRPar,TLPar,TChar '_',TRPar,TIdentifier "_75_39cG94iB6M_1_591g_q_5_8Bq6Q86___K49442__",TBool True,TLPar,TLPar,TIdentifier "V5__6QX7",TChar '"',TInt 5,TBool True,TKeyword "b",TKeyword "NrnIQZWHNXeTLZcCGQDKUUlCXZKKSKtqZFZoDoULGyhcZEwwKsNXKcdosCGq",TChar ' ',TLPar,TInt (-49),TKeyword "LqAYGfukTjIrbwyMEjTzHYQeCxlsuashlAGrJYYnmV",TLPar,TInt (-14),TOperator "+@-",TInt 13,TKeyword "VXxnmmnsPmgFVdDfmJdPOyUT",TInt 18,TInt 9,TChar '*',TOperator "$=<",TInt (-9),TInt (-16),TInt 23,TString "\\r\\t0\\r=m<76e-^M95*6m\\r 36 ~ \\t\\nR4=\\to\\n$ \\n!Zk9\\r5",TIdentifier "_____84F_98516Y43__AuN__nA_V1G_50wQ_FC85XR53f6K_e_H",TOperator "+",TString "/\\r9+I\\n \\t\\r\\t1S>C",TKeyword "njwEXIgAMgrG",TIdentifier "_z_3d6_4j_w0_6_E2I0__ixKe3___S1_yN6iG_XS_V2j9o935OI_0265795e2",TChar '\n',TKeyword "ddDSvElwXOYJFdWlqWexkYGQJbfVAXjUoaDWURkKpvkDBe",TIdentifier "_V8_a__Z2__Jd____wa64__1__29V386_U",TKeyword "IcUdsXyptcoJaAwqTYmPXtnrXSBrWJxBxprhnHmjwqvDyywuRStfeLKoyxcRol"]

r106t = [TOperator "=.?;",TLPar,TKeyword "ysSURgFtK",TRPar,TOperator ".:#>",TOperator ",",TKeyword "bVLkbsDOrBDanJICrVSYFsqFIkPcWZpnrrDUaFVTcMwToMEuhyR",TLPar,TString "\n/\t\t&\n0>\n",TLPar,TString "\t\n7<J9u3?/;\t\n\r9\t|\t*.\t8I",TIdentifier "W_08t_4__1_Hq1L____O9_",TRPar,TLPar,TChar '_',TRPar,TIdentifier "_75_39cG94iB6M_1_591g_q_5_8Bq6Q86___K49442__",TBool True,TLPar,TLPar,TIdentifier "V5__6QX7",TChar '"',TInt 5,TBool True,TKeyword "b",TKeyword "NrnIQZWHNXeTLZcCGQDKUUlCXZKKSKtqZFZoDoULGyhcZEwwKsNXKcdosCGq",TChar 'a', TChar ' ',TLPar,TInt (-49),TKeyword "LqAYGfukTjIrbwyMEjTzHYQeCxlsuashlAGrJYYnmV",TLPar,TInt (-14),TOperator "+@-",TInt 13,TKeyword "VXxnmmnsPmgFVdDfmJdPOyUT",TInt 18,TInt 9,TChar '*',TOperator "$=<",TInt (-9),TInt (-16),TInt 23,TString "\r\t0\r=m<76e-^M95*6m\r 36 ~ \t\nR4=\to\n$ \n!Zk9\r5",TIdentifier "_____84F_98516Y43__AuN__nA_V1G_50wQ_FC85XR53f6K_e_H",TOperator "+",TString "/\r9+I\n \t\r\t1S>C",TKeyword "njwEXIgAMgrG",TIdentifier "_z_3d6_4j_w0_6_E2I0__ixKe3___S1_yN6iG_XS_V2j9o935OI_0265795e2",TChar '\n',TKeyword "ddDSvElwXOYJFdWlqWexkYGQJbfVAXjUoaDWURkKpvkDBe",TIdentifier "_V8_a__Z2__Jd____wa64__1__29V386_U",TKeyword "IcUdsXyptcoJaAwqTYmPXtnrXSBrWJxBxprhnHmjwqvDyywuRStfeLKoyxcRol"]


r107t = []
r107 = nonDetTokenize ["ask"] [] "ask"

r108 = nonDetTokenize ["ask"] [] "askance"

-- (p >>= \a -> ((k a) <|> (h a))) `parserEq` ((p >>= k) <|> (p >>= h))

data Kind = A | B deriving (Eq, Show)
data TTok =
    TKey Kind String
    | TId Kind String
    deriving (Eq, Show)

r109p = oneCharStr <|> twoCharStr
oneCharStr = oneChar >>= \c -> return [c]
twoCharStr = oneChar >>= \c1 -> oneChar >>= \c2 -> return [c1,c2]

newtype T = T String deriving (Show, Eq)
-- literal' :: String -> StringParser SpecialString
-- literal' "" = return $ SpecialString ""
-- literal' (c:cs) = do { a <- literalChar c ; x <- literal' cs; return SpecialString (a:x) }

r109k s = (literal $ map toUpper s) >>= \s' -> return $ T s'

r109s = oneCharStr >>= r109k

r110p = r109p

r110k "a" | trace ("r110k " ++ show "a") True = literalChar 'x' >> return 1
r110k "ab" | trace ("r110k " ++ show "ab") True = literalChar 'y' >> return 2
r110k x | trace ("r110k " ++ show x) True = return 3

r110s = oneCharStr >>= r110k

r111s = r110p >>= r110k

r112p = r110p

r112k "a" | trace ("r112k " ++ show "a") True = (literalChar 'b' >> return 1) <|> (literalChar 'y' >> return 2)
r112k "ab" | trace ("r112k " ++ show "ab") True = (literalChar 'u' >> return 3) <|> (literalChar 'v' >> return 4)
r112k x | trace ("r112k " ++ show x) True = return 5

r112s = r112p >>= r112k

r113p = r110p

r113k "a" | trace ("r113k " ++ show "a") True = (literalChar 'b' >> return 1) <|> (literalChar 'y' >> return 2)
r113k "ab" | trace ("r113k " ++ show "ab") True = (literalChar 'u' >> return 3) <|> (literalChar 'v' >> return 4)
r113k x | trace ("r113k " ++ show x) True = return 5

r113h "a" | trace ("r113h " ++ show "a") True = (literalChar 'b' >> return 6) <|> (literalChar 'y' >> return 7)
r113h "ab" | trace ("r113h " ++ show "ab") True = (literalChar 'u' >> return 8) <|> (literalChar 'v' >> return 9)
r113h x | trace ("r113h " ++ show x) True = return 10

r113s = r113p >>= \s -> (r113k s <|> r113h s)

r113s' = (r113p >>= r113k) <|> (r113p >>= r113h)


-- r109l = (p >>= \a -> ((k a) <|> (h a)))
--     where
--         p = r109p
--         k a =
--
-- r109r = (p >>= k) <|> (p >>= h)
--     where
--         p = r109p
--         k ((TKeyword s):_) = tokenizer ["one-"++s] []
--         k ((TIdentifier s):_) = tokenizer ["two-"++s] []
--         h ((TKeyword s):_) = tokenizer ["uno-"++s] []
--         h ((TIdentifier s):_) = tokenizer ["due-"++s] []
--
-- r109l'= parse r109l "ask one-a"


data TA =
    TA {
        kwds :: [String],
        ops :: [String],
        str :: String,
        res :: [([Token],String)],
        toks :: [Token],
        zs :: [(Token,Token)]
    }
    deriving Show

untoktok ts =
    TA { kwds = k, ops = o, str = s, res = r, toks = t, zs = z }
    where
        k = map keywordTokenString $ filter isKeywordToken ts
        o = map operatorTokenString $ filter isOperatorToken ts
        s = untokenize ts
        r = tokenize k o s
        t = fst . head $ r
        z = zip ts t

dispcomp :: [Token] -> IO ()
dispcomp ts = display $ zs $ untoktok ts



display :: (Show a,Eq a) => [(a,a)] -> IO ()
display zs = sequence_ [putStrLn (show a ++ rel (a,b) ++ show b) | (a,b) <- zs]
    where
        rel (a,b) | a == b = " = "
        rel (a,b)          = " /= "


main = do
    putStrLn "Hello Mparseco"
