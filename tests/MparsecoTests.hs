module MparsecoTests (tests) where

import Mparseco
import MparsecoTests.Generators
import MparsecoTests.Basic
import Distribution.TestSuite.QuickCheck

test1_1 = testProperty "prop1 simpleLetterString" $ prop1 simpleLetterString
test1_2 = testProperty "prop1 letterString" $ prop1 letterString
test1_3 = testProperty "prop1 digitString" $ prop1 digitString
test1_4 = testProperty "prop1 alphaNumString" $ prop1 alphaNumString
test1_5 = testProperty "prop1 normalString" $ prop1 normalString
test1_6 = testProperty "prop1 arbitraryASCIIString" $ prop1 arbitraryASCIIString
test1_7 = testProperty "prop1 arbitraryPrintableString" $ prop1 arbitraryPrintableString
test1_8 = testProperty "prop1 arbitraryUnicodeString" $ prop1 arbitraryUnicodeString

tests :: IO [Test]
tests = return
    [
        test1_1,
        test1_2,
        test1_3,
        test1_4,
        test1_5,
        test1_6,
        test1_7,
        test1_8
    ]
