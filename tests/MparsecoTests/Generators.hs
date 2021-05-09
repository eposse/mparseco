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
    arbitraryUnicodeString
)
where

import Test.QuickCheck

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
otherSymbol         = elements ['~','@','#','$','%','^','&','_','|','|']
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
