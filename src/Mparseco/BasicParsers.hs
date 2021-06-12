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

oneChar :: MParser Char
oneChar = MParser trans
    where
        trans [] = []
        trans (c:cs) = [(c,cs)]

letter :: MParser Char
letter = oneChar |> isLetter

digitChar :: MParser Char
digitChar = oneChar |> isDigit

digit :: MParser Int
-- digit = (oneChar |> isDigit) >>= \c -> return (ord c - (ord '0'))
digit = do
    c <- digitChar
    return $ ord c - (ord '0')

literalChar :: Char -> MParser Char
literalChar c = oneChar |> (== c)

naturalNumberLiteral :: MParser String
naturalNumberLiteral = do
    d <- digitChar
    ds <- maxZeroOrMore digitChar
    return $ (d:ds)

naturalNumberLiterals :: MParser String
naturalNumberLiterals = do
    d <- digitChar
    ds <- allZeroOrMore digitChar
    return $ (d:ds)

naturalNumbers' :: MParser Int
-- naturalNumbers' = allZeroOrMore digit >>= \ds -> return (digitsToNat ds)
naturalNumbers' = do
    ds <- allZeroOrMore digit
    return $ digitsToNat ds

naturalNumbers :: MParser Int
-- naturalNumbers = digit >>= \d -> allZeroOrMore digit >>= \ds -> return (digitsToNat (d:ds))
naturalNumbers = do
    d <- digit
    ds <- allZeroOrMore digit
    return $ digitsToNat (d:ds)

naturalNumber :: MParser Int
-- naturalNumber = digit >>= \d -> maxZeroOrMore digit >>= \ds -> return (digitsToNat (d:ds))
naturalNumber = do
    d <- digit
    ds <- maxZeroOrMore digit
    return $ digitsToNat (d:ds)

identifiers :: MParser String
identifiers = do
    l <- letter
    ls <- allZeroOrMore (letter <|> digitChar <|> (literalChar '_'))
    return (l:ls)

identifier :: MParser String
identifier = do
    l <- letter
    ls <- maxZeroOrMore (letter <|> digitChar <|> (literalChar '_'))
    return (l:ls)

literal :: String -> MParser String
literal "" = return ""
literal (c:cs) = do { a <- literalChar c ; x <- literal cs; return (a:x) }

spaces :: MParser String
spaces = maxOneOrMore (oneChar |> isSpace)
