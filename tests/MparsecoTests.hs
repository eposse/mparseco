module MparsecoTests (tests) where

import Mparseco
import Test.QuickCheck
import Distribution.TestSuite.QuickCheck

simpleLetterString = listOf $ choose ('a','d')
letterString = listOf $ choose ('a','z')
digitString = listOf $ choose ('0','9')

prop1 = \s -> case s of {[] -> True; (c:cs) -> parse oneChar s == [(c,cs)]}

test1 = testProperty "prop1" prop1

tests :: IO [Test]
tests = return [ test1 ]
