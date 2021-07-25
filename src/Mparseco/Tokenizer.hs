module Mparseco.Tokenizer
(
    Token(..),
    isBoolToken,
    isCharToken,
    isIntToken,
    isStringToken,
    isKeywordToken,
    isOperatorToken,
    isIdentifierToken,
    isLParToken,
    isRParToken,
    boolTokenValue,
    intTokenValue,
    stringTokenValue,
    keywordTokenString,
    operatorTokenString,
    identifierTokenString,
    next,
    nextBasicToken,
    nextToken,
    nextPossibleTokens,
    basicToken,
    token,
    possibleTokens,
    basicTokenizer,
    tokenizer,
    nonDetTokenizer,
    basicTokenize,
    tokenize,
    nonDetTokenize,
    boolToken,
    charToken,
    intToken,
    stringToken,
    identifierToken,
    keywordToken,
    operatorToken,
    lparToken,
    rparToken,
    unescape,
    untoken,
    untokenize
)
where

import Mparseco.Core
import Mparseco.BasicParsers
import Debug.Trace

data Token =
      TBool Bool
    | TChar Char
    | TInt Int
    | TString String
    | TKeyword String
    | TOperator String
    | TIdentifier String
    | TLPar
    | TRPar
    deriving (Eq, Show)

isBoolToken (TBool _) = True
isBoolToken _ = False

isIntToken (TInt _) = True
isIntToken _ = False

isCharToken (TChar _) = True
isCharToken _ = False

isStringToken (TString _) = True
isStringToken _ = False

isKeywordToken (TKeyword _) = True
isKeywordToken _ = False

isOperatorToken (TOperator _) = True
isOperatorToken _ = False

isIdentifierToken (TIdentifier _) = True
isIdentifierToken _ = False

isLParToken TLPar = True
isLParToken _ = False

isRParToken TRPar = True
isRParToken _ = False

boolTokenValue (TBool b) = b
charTokenValue (TChar c) = c
intTokenValue (TInt i) = i
stringTokenValue (TString s) = s
keywordTokenString (TKeyword k) = k
operatorTokenString (TOperator o) = o
identifierTokenString (TIdentifier i) = i

next :: StringParser Token -> StringParser Token
next token =
    do
        spaces
        token
    </>
        token

nextBasicToken:: StringParser Token
nextBasicToken= next basicToken

nextToken :: [String] -> [String] -> StringParser Token
nextToken kwds ops = next $ token kwds ops

nextPossibleTokens :: [String] -> [String] -> StringParser Token
nextPossibleTokens kwds ops = next $ possibleTokens kwds ops

basicToken :: StringParser Token
basicToken = oneof [boolToken, charToken, intToken, stringToken, identifierToken, lparToken, rparToken]

token :: [String] -> [String] -> StringParser Token
token kwds ops = do
    (oneof [keywordToken k | k <- kwds])
    </>
    (oneof [operatorToken op | op <- ops])
    </>
    basicToken

possibleTokens :: [String] -> [String] -> StringParser Token
possibleTokens kwds ops = do
    (oneof [keywordToken k | k <- kwds])
    <|>
    (oneof [operatorToken op | op <- ops])
    <|>
    basicToken

basicTokenizer :: StringParser [Token]
basicTokenizer = maxZeroOrMore nextBasicToken

tokenizer :: [String] -> [String] -> StringParser [Token]
tokenizer kwds ops = maxZeroOrMore $ nextToken kwds ops

nonDetTokenizer :: [String] -> [String] -> StringParser [Token]
nonDetTokenizer kwds ops = maxZeroOrMore $ nextPossibleTokens kwds ops

basicTokenize :: String -> [([Token], String)]
basicTokenize = parse basicTokenizer

tokenize :: [String] -> [String] -> String -> [([Token], String)]
tokenize kwds ops = parse $ tokenizer kwds ops

nonDetTokenize :: [String] -> [String] -> String -> [([Token], String)]
nonDetTokenize kwds ops = parse $ nonDetTokenizer kwds ops

boolToken :: StringParser Token
boolToken =
        do { literal "true"; return $ TBool True }
    <|>
        do { literal "false"; return $ TBool False }

charToken :: StringParser Token
charToken = do
    literalChar '\''
    c <- oneChar
    literalChar '\''
    return $ TChar c

intToken :: StringParser Token
intToken = do
        do { n <- naturalNumber; return $ TInt n }
    <|>
        do { literalChar '-'; n <- naturalNumber; return $ TInt (-n) }
    <|>
        do { literalChar '+'; n <- naturalNumber; return $ TInt n }

stringToken :: StringParser Token
stringToken = do
    literalChar '"'
    s <- oneChar `while` (/= '"')
    literalChar '"'
    return $ TString s

identifierToken :: StringParser Token
identifierToken = do
    name <- identifier
    return $ TIdentifier name

keywordToken :: String -> StringParser Token
keywordToken s = do
    lit <- literal s
    return $ TKeyword lit

operatorToken :: String -> StringParser Token
operatorToken s = do
    lit <- literal s
    return $ TOperator lit

lparToken :: StringParser Token
lparToken = do
    literalChar '('
    return $ TLPar

rparToken :: StringParser Token
rparToken = do
    literalChar ')'
    return $ TRPar

unescape :: String -> String
unescape s | trace ("unescape " ++ show s ++ "") False = undefined
unescape s = read $ "\"" ++ s ++ "\""

untoken :: Token -> String
untoken (TBool True)    = "true"
untoken (TBool False)   = "false"
untoken (TInt i)        = show i
untoken (TChar c)       = unescape $ show c
untoken (TString s)     = unescape $ s -- "\"" ++ s ++ "\""
untoken (TLPar)         = "("
untoken (TRPar)         = ")"
untoken (TIdentifier n) = n
untoken (TKeyword k)    = k
untoken (TOperator o)   = o

untokenize :: [Token] -> String
untokenize l = (foldl1 (\x y -> x ++ " " ++ y) (map untoken l))
