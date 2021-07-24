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
    arbitraryKeyword,
    arbitraryOperator,
    arbitraryIdentifier,
    tokeng,
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
nonQuotSymbols      = oneof [ arithOp, relOp, brackets, punctuation, otherSymbol ]
symbols             = oneof [ arithOp, relOp, brackets, punctuation, quotes, otherSymbol ]
spaces              = elements [' ','\t','\n','\r']
normalChar          = oneof [ alphaNum, symbols, spaces ]

simpleLetterString          = listOf1 $ choose ('a','d')
letterString                = listOf1 $ letters
digitString                 = listOf1 $ digits
alphaNumString              = listOf1 $ alphaNum
symbolString                = listOf1 $ symbols
normalString                = listOf $ normalChar
arbitraryASCIIString        = listOf $ arbitraryASCIIChar
arbitraryPrintableString    = listOf $ arbitraryPrintableChar
arbitraryUnicodeString      = listOf $ arbitraryUnicodeChar

identifierChar :: Gen Char
identifierChar =
    oneof
    [
        elements ['_'],
        letters,
        digits
    ]

identifierString :: Gen String
identifierString =
    do
        first <- oneof [ choose ('_','_'), letters ]
        rest <- listOf identifierChar
        return $ (first:rest)

arbitraryKeyword = letterString
arbitraryOperator = scale (min 4) $ listOf1 nonQuotSymbols
arbitraryIdentifier = identifierString


tokeng :: Gen Token
tokeng =
    oneof
    [
        do { b <- arbitrary; return $ TBool b },
        do { i <- arbitrary; return $ TInt i },
        do { c <- arbitrary; return $ TChar c },
        do { s <- arbitrary; return $ TString s },
        do { k <- arbitraryKeyword; return $ TKeyword k },
        do { o <- arbitraryOperator; return $ TOperator o },
        do { i <- arbitraryIdentifier; return $ TIdentifier i },
        do { return $ TLPar },
        do { return $ TRPar }
    ]

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
