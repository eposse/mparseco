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
    arbitraryToken,
    arbitraryTokens,
    arbitraryTokensKwdsOps,
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
operatorSymbols      = oneof [ arithOp, relOp, punctuation, otherSymbol ]
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
arbitraryOperator = scale (min 4) $ listOf1 operatorSymbols
arbitraryIdentifier = identifierString


arbitraryToken :: Gen Token
arbitraryToken =
    oneof
    [
        do { b <- arbitrary; return $ TBool b },
        do { i <- arbitrary; return $ TInt i },
        do { c <- normalChar; return $ TChar c },
        do { s <- normalString; return $ TString s },
        do { k <- arbitraryKeyword; return $ TKeyword k },
        do { o <- arbitraryOperator; return $ TOperator o },
        do { i <- arbitraryIdentifier; return $ TIdentifier i },
        do { return $ TLPar },
        do { return $ TRPar }
    ]

arbitraryTokens :: Gen [Token]
arbitraryTokens = listOf arbitraryToken

arbitraryTokensKwdsOps :: Gen ([Token], [Token], [Token])
arbitraryTokensKwdsOps =
    do
        tokens <- listOf arbitraryToken
        let kwds = filter isKeywordToken tokens
        let ops = filter isOperatorToken tokens
        return $ (tokens, kwds, ops)

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
