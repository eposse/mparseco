module Mparseco.BasicParsers
(
    oneChar,
    letter,
    digit,
    literalChar,
    naturalNumbers,
    naturalNumber
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

digit :: MParser Int
-- digit = (oneChar |> isDigit) >>= \c -> return (ord c - (ord '0'))
digit = do
    c <- (oneChar |> isDigit)
    return $ ord c - (ord '0')

literalChar :: Char -> MParser Char
literalChar c = oneChar |> (== c)

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
