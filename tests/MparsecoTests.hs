module Main (main) where

import Mparseco
import Test.QuickCheck

simpleLetterString = listOf $ choose ('a','d')
letterString = listOf $ choose ('a','z')
digitString = listOf $ choose ('0','9')

prop1 = \s -> case s of {[] -> True; (c:cs) -> parse oneChar s == [(c,cs)]}

test1 = quickCheck prop1

main :: IO ()
main = do
    putStrLn $ "test1 == "
    test1
