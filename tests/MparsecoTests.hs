module MparsecoTests (tests) where

import Mparseco
import MparsecoTests.Generators
import MparsecoTests.BasicProperties
import Distribution.TestSuite.QuickCheck
import Data.Char

generators =
    [
        ("simpleLetterString", simpleLetterString),
        ("letterString", letterString),
        ("digitString", digitString),
        ("alphaNumString", alphaNumString),
        ("normalString", normalString),
        ("arbitraryASCIIString", arbitraryASCIIString),
        ("arbitraryPrintableString", arbitraryPrintableString),
        ("arbitraryUnicodeString", arbitraryUnicodeString)
    ]

prop1_tests = [ testProperty ("prop1 " ++ label) $ prop1 g | (label, g) <- generators ]

prop2_tests =
    [ testProperty ("prop2" ++ label ++ " 'a' literalChar") $ prop2 g 'a' literalChar | (label, g) <- generators ]

prop3_tests =
    [ testProperty ("prop3" ++ label ++ " oneChar") $ prop3 g oneChar | (label, g) <- generators ]

prop4_tests =
    [ testProperty ("prop4" ++ label ++ " oneChar literalChar literalChar") $ prop4 g oneChar literalChar literalChar | (label, g) <- generators ]

prop5_tests =
    [ testProperty ("prop5 " ++ label ++ " oneChar") $ prop5 g oneChar | (label, g) <- generators ]

prop6_tests =
    [ testProperty ("prop6 " ++ label ++ " oneChar") $ prop6 g oneChar | (label, g) <- generators ]

prop7_tests =
    [ testProperty ("prop7 " ++ label ++ " oneChar oneChar oneChar") $ prop7 g oneChar oneChar oneChar | (label, g) <- generators ]

prop8_tests =
    [ testProperty ("prop8 " ++ label ++ " literalChar") $ prop8 g literalChar | (label, g) <- generators ]

prop9_tests =
    [ testProperty ("prop9 " ++ label ++ " oneChar") $ prop9 g oneChar | (label, g) <- generators ]


tests :: IO [Test]
tests = do
    -- printSamples
    return $
        prop1_tests ++
        prop2_tests ++
        prop3_tests ++
        prop4_tests ++
        prop5_tests ++
        prop6_tests ++
        prop7_tests ++
        prop8_tests ++
        prop9_tests
