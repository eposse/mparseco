module MparsecoTests.Generators
(
    lowercaseLetters,
    uppercaseLetters,
    letters,
    digits,
    alphaNum,
    arithOp,
    relOp,
    brackets,
    punctuation,
    quotes,
    otherSymbol,
    symbols,
    spaces,
    normalChar,
    simpleLetterString,
    letterString,
    digitString,
    alphaNumString,
    symbolString,
    normalString,
    arbitraryASCIIString,
    arbitraryPrintableString,
    arbitraryUnicodeString,
    printSamples
)
where

import Test.QuickCheck
import Mparseco.Tokenizer

lowercaseLetters    = choose ('a','z')
uppercaseLetters    = choose ('A','Z')
letters             = oneof [ lowercaseLetters, uppercaseLetters ]
digits              = choose ('0','9')
alphaNum            = oneof [ letters, digits ]
arithOp             = elements ['+','-','*','/']
relOp               = elements ['<','>','=']
brackets            = elements ['(',')','[',']','{','}']
punctuation         = elements ['.',',',':',';','!','?']
quotes              = elements ['\'','\"','`']
otherSymbol         = elements ['~','@','#','$','%','^','&','_','|']
symbols             = oneof [ arithOp, relOp, brackets, punctuation, quotes, otherSymbol ]
spaces              = elements [' ','\t','\n','\r']
normalChar          = oneof [ alphaNum, symbols, spaces ]

simpleLetterString          = listOf $ choose ('a','d')
letterString                = listOf $ letters
digitString                 = listOf $ digits
alphaNumString              = listOf $ alphaNum
symbolString                = listOf $ symbols
normalString                = listOf $ normalChar
arbitraryASCIIString        = listOf $ arbitraryASCIIChar
arbitraryPrintableString    = listOf $ arbitraryPrintableChar
arbitraryUnicodeString      = listOf $ arbitraryUnicodeChar

-- TODO: ADD Token generators

-- tokeng :: Gen Token
-- tokeng =
--     oneof
--     [
--         return $ TBool arbitrary,
--         return $ TInt arbitrary
--     ]

printSamples = do
    putStrLn "* letterString sample:"
    sample letterString
    putStrLn "* digitString sample:"
    sample digitString
    putStrLn "* alphaNumString sample:"
    sample alphaNumString
    putStrLn "* normalString sample:"
    sample normalString
    putStrLn "* arbitraryASCIIString sample:"
    sample arbitraryASCIIString
    putStrLn "* arbitraryPrintableString sample:"
    sample arbitraryPrintableString
    putStrLn "* arbitraryUnicodeString sample:"
    sample arbitraryUnicodeString
