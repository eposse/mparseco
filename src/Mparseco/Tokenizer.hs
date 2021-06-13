module Mparseco.Tokenizer
(
    Token(..),
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
    untoken,
    untokenize
)
where

import Mparseco.Core
import Mparseco.BasicParsers

data Token =
      TBool Bool
    | TChar Char
    | TInt Int
    | TString String
    | TKeyword String
    | TIdentifier String
    | TLPar
    | TRPar
    | TOperator String
    deriving (Eq, Show)

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
    spaces
    return $ TKeyword lit

operatorToken :: String -> StringParser Token
operatorToken s = do
    lit <- literal s
    spaces
    return $ TOperator lit

lparToken :: StringParser Token
lparToken = do
    literalChar '('
    return $ TLPar

rparToken :: StringParser Token
rparToken = do
    literalChar ')'
    return $ TRPar

untoken :: Token -> String
untoken (TBool True)    = "true"
untoken (TBool False)   = "false"
untoken (TInt i)        = show i
untoken (TChar c)       = show c
untoken (TString s)     = show s
untoken (TLPar)         = "("
untoken (TRPar)         = ")"
untoken (TIdentifier n) = n
untoken (TKeyword k)    = k
untoken (TOperator o)   = o

untokenize :: [Token] -> String
untokenize l = foldl (\x y -> x ++ " " ++ y) ""  (map untoken l)
