module Mparseco.Utils
(
    digitsToNat,
    digitStringToNat
)
where

import Data.Char
-- import Debug.Trace

digitStringToNat :: String -> Int
digitStringToNat s = digitStringToNat' s 0
    where
        -- digitsToNat' l r | trace ("digitStringToNat " ++ show l ++ " " ++ show r) False = undefined
        digitStringToNat' [] r = r
        digitStringToNat' (c:cs) r | isDigit c = digitStringToNat' cs (10 * r + digitToInt c)

digitsToNat :: [Int] -> Int
digitsToNat s = digitsToNat' s 0
    where
        -- digitsToNat' l r | trace ("digitsToNat " ++ show l ++ " " ++ show r) False = undefined
        digitsToNat' [] r = r
        digitsToNat' (d:ds) r = digitsToNat' ds (10 * r + d)
