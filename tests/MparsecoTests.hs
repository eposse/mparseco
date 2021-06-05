module MparsecoTests (tests) where

import Mparseco
import MparsecoTests.Generators
import MparsecoTests.Properties
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

prop_seq_1_tests =
    [ testProperty ("prop_seq_1 " ++ label ++ " oneChar literalChar") $ prop_seq_1 g oneChar literalChar | (label, g) <- generators ] ++
    [ testProperty ("prop_seq_1 digitString naturalNumbers (\n -> naturalNumber >>= \\m -> return (n,m))") $ prop_seq_1 digitString naturalNumbers (\n -> naturalNumber >>= \m -> return (n,m)) ]

prop_seq_2_tests =
    [ testProperty ("prop_seq_2 " ++ label ++ " 'a' literalChar") $ prop_seq_2 g 'a' literalChar | (label, g) <- generators ]

prop_seq_3_tests =
    [ testProperty ("prop_seq_3 " ++ label ++ " oneChar") $ prop_seq_3 g oneChar | (label, g) <- generators ]

prop_seq_4_tests =
    [ testProperty ("prop_seq_4 " ++ label ++ " oneChar literalChar literalChar") $ prop_seq_4 g oneChar literalChar literalChar | (label, g) <- generators ]

prop_alt_1_tests =
    [ testProperty ("prop_alt_1 " ++ label ++ " oneChar oneChar") $ prop_alt_1 g oneChar oneChar | (label, g) <- generators ] ++
    [ testProperty ("prop_alt_1 alphaNumString naturalNumberLiterals identifiers") $ prop_alt_1 alphaNumString naturalNumberLiterals identifier]

prop_alt_2_tests =
    [ testProperty ("prop_alt_2 " ++ label ++ " oneChar") $ prop_alt_2 g oneChar | (label, g) <- generators ]

prop_alt_3_tests =
    [ testProperty ("prop_alt_3 " ++ label ++ " oneChar") $ prop_alt_3 g oneChar | (label, g) <- generators ]

prop_alt_4_tests =
    [ testProperty ("prop_alt_4 " ++ label ++ " oneChar oneChar oneChar") $ prop_alt_4 g oneChar oneChar oneChar | (label, g) <- generators ]

prop_alt_seq_1_tests =
    [ testProperty ("prop_alt_seq_1 " ++ label ++ " literalChar") $ prop_alt_seq_1 g literalChar | (label, g) <- generators ]

prop_alt_seq_2_tests =
    [ testProperty ("prop_alt_seq_2 " ++ label ++ " oneChar") $ prop_alt_seq_2 g oneChar | (label, g) <- generators ]

prop_alt_seq_3_tests =
    [ testProperty ("prop_alt_seq_3 " ++ label ++ " oneChar oneChar literalChar") $ prop_alt_seq_3 g oneChar oneChar literalChar | (label, g) <- generators ]

prop_alt_seq_4_tests =
    [ testProperty ("prop_alt_seq_4 " ++ label ++ " oneChar literalChar literalChar") $ prop_alt_seq_4 g oneChar literalChar literalChar | (label, g) <- generators ]

prop_filt_alt_1_tests =
    [ testProperty ("prop_filt_alt_1 " ++ label ++ " isLetter") $ prop_filt_alt_1 g isLetter | (label, g) <- generators ]

prop_filt_alt_2_tests =
    [ testProperty ("prop_filt_alt_2 " ++ label ++ " oneChar oneChar isLetter") $ prop_filt_alt_2 g oneChar oneChar isLetter | (label, g) <- generators ]

prop_bchoice_seq_1_tests =
    [ testProperty ("prop_bchoice_seq_1" ++ label ++ " oneChar literalChar literalChar") $ prop_bchoice_seq_1 g oneChar literalChar literalChar | (label, g) <- generators ]


tests :: IO [Test]
tests = do
    -- printSamples
    return $
        prop1_tests ++
        prop_seq_1_tests ++
        prop_seq_2_tests ++
        prop_seq_3_tests ++
        prop_seq_4_tests ++
        prop_alt_1_tests ++
        prop_alt_2_tests ++
        prop_alt_3_tests ++
        prop_alt_4_tests ++
        prop_alt_seq_1_tests ++
        prop_alt_seq_2_tests ++
        prop_alt_seq_3_tests ++
        prop_alt_seq_4_tests ++
        prop_filt_alt_1_tests ++
        prop_filt_alt_2_tests ++
        prop_bchoice_seq_1_tests
