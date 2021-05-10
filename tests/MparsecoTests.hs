module MparsecoTests (tests) where

import Mparseco
import MparsecoTests.Generators
import MparsecoTests.BasicProperties
import Distribution.TestSuite.QuickCheck

test1_1 = testProperty "prop1 simpleLetterString" $ prop1 simpleLetterString
test1_2 = testProperty "prop1 letterString" $ prop1 letterString
test1_3 = testProperty "prop1 digitString" $ prop1 digitString
test1_4 = testProperty "prop1 alphaNumString" $ prop1 alphaNumString
test1_5 = testProperty "prop1 normalString" $ prop1 normalString
test1_6 = testProperty "prop1 arbitraryASCIIString" $ prop1 arbitraryASCIIString
test1_7 = testProperty "prop1 arbitraryPrintableString" $ prop1 arbitraryPrintableString
test1_8 = testProperty "prop1 arbitraryUnicodeString" $ prop1 arbitraryUnicodeString

test2_1 = testProperty "prop2 simpleLetterString oneChar" $ prop2 simpleLetterString oneChar
test2_2 = testProperty "prop2 letterString oneChar" $ prop2 letterString oneChar
test2_3 = testProperty "prop2 digitString oneChar" $ prop2 digitString oneChar
test2_4 = testProperty "prop2 alphaNumString oneChar" $ prop2 alphaNumString oneChar
test2_5 = testProperty "prop2 normalString oneChar" $ prop2 normalString oneChar
test2_6 = testProperty "prop2 arbitraryASCIIString oneChar" $ prop2 arbitraryASCIIString oneChar
test2_7 = testProperty "prop2 arbitraryPrintableString oneChar" $ prop2 arbitraryPrintableString oneChar
test2_8 = testProperty "prop2 arbitraryUnicodeString oneChar" $ prop2 arbitraryUnicodeString oneChar


tests :: IO [Test]
tests = do
    -- printSamples
    return
        [
            test1_1,
            test1_2,
            test1_3,
            test1_4,
            test1_5,
            test1_6,
            test1_7,
            test1_8,
            test2_1,
            test2_3,
            test2_3,
            test2_4,
            test2_6,
            test2_6,
            test2_7,
            test2_8
        ]
