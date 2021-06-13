module Mparseco.BasicParsers
(
    oneChar,
    letter,
    digit,
    literalChar,
    naturalNumberLiteral,
    naturalNumberLiterals,
    naturalNumbers,
    naturalNumber,
    identifiers,
    identifier,
    literal,
    spaces
)
where

import Data.Char
import Mparseco.Core --((|>),(<|>),empty)
import Mparseco.Utils

oneChar :: StringParser Char
oneChar = MParser trans
    where
        trans [] = []
        trans (c:cs) = [(c,cs)]

letter :: StringParser Char
letter = oneChar |> isLetter

digitChar :: StringParser Char
digitChar = oneChar |> isDigit

digit :: StringParser Int
-- digit = (oneChar |> isDigit) >>= \c -> return (ord c - (ord '0'))
digit = do
    c <- digitChar
    return $ ord c - (ord '0')

literalChar :: Char -> StringParser Char
literalChar c = oneChar |> (== c)

naturalNumberLiteral :: StringParser String
naturalNumberLiteral = do
    d <- digitChar
    ds <- maxZeroOrMore digitChar
    return $ (d:ds)

naturalNumberLiterals :: StringParser String
naturalNumberLiterals = do
    d <- digitChar
    ds <- allZeroOrMore digitChar
    return $ (d:ds)

naturalNumbers' :: StringParser Int
-- naturalNumbers' = allZeroOrMore digit >>= \ds -> return (digitsToNat ds)
naturalNumbers' = do
    ds <- allZeroOrMore digit
    return $ digitsToNat ds

naturalNumbers :: StringParser Int
-- naturalNumbers = digit >>= \d -> allZeroOrMore digit >>= \ds -> return (digitsToNat (d:ds))
naturalNumbers = do
    d <- digit
    ds <- allZeroOrMore digit
    return $ digitsToNat (d:ds)

naturalNumber :: StringParser Int
-- naturalNumber = digit >>= \d -> maxZeroOrMore digit >>= \ds -> return (digitsToNat (d:ds))
naturalNumber = do
    d <- digit
    ds <- maxZeroOrMore digit
    return $ digitsToNat (d:ds)

identifiers :: StringParser String
identifiers = do
    l <- letter
    ls <- allZeroOrMore (letter <|> digitChar <|> (literalChar '_'))
    return (l:ls)

identifier :: StringParser String
identifier = do
    l <- letter
    ls <- maxZeroOrMore (letter <|> digitChar <|> (literalChar '_'))
    return (l:ls)

literal :: String -> StringParser String
literal "" = return ""
literal (c:cs) = do { a <- literalChar c ; x <- literal cs; return (a:x) }

spaces :: StringParser String
spaces = maxOneOrMore (oneChar |> isSpace)
