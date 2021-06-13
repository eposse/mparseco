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
    rparToken
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

next :: MParser Token -> MParser Token
next token =
    do
        spaces
        token
    </>
        token

nextBasicToken:: MParser Token
nextBasicToken= next basicToken

nextToken :: [String] -> [String] -> MParser Token
nextToken kwds ops = next $ token kwds ops

nextPossibleTokens :: [String] -> [String] -> MParser Token
nextPossibleTokens kwds ops = next $ possibleTokens kwds ops

basicToken :: MParser Token
basicToken = oneof [boolToken, charToken, intToken, stringToken, identifierToken, lparToken, rparToken]

token :: [String] -> [String] -> MParser Token
token kwds ops = do
    (oneof [keywordToken k | k <- kwds])
    </>
    (oneof [operatorToken op | op <- ops])
    </>
    basicToken

possibleTokens :: [String] -> [String] -> MParser Token
possibleTokens kwds ops = do
    (oneof [keywordToken k | k <- kwds])
    <|>
    (oneof [operatorToken op | op <- ops])
    <|>
    basicToken

basicTokenizer :: MParser [Token]
basicTokenizer = maxZeroOrMore nextBasicToken

tokenizer :: [String] -> [String] -> MParser [Token]
tokenizer kwds ops = maxZeroOrMore $ nextToken kwds ops

nonDetTokenizer :: [String] -> [String] -> MParser [Token]
nonDetTokenizer kwds ops = maxZeroOrMore $ nextPossibleTokens kwds ops

basicTokenize :: String -> [([Token], State)]
basicTokenize = parse basicTokenizer

tokenize :: [String] -> [String] -> String -> [([Token], State)]
tokenize kwds ops = parse $ tokenizer kwds ops

nonDetTokenize :: [String] -> [String] -> String -> [([Token], State)]
nonDetTokenize kwds ops = parse $ nonDetTokenizer kwds ops

boolToken :: MParser Token
boolToken =
        do { literal "true"; return $ TBool True }
    <|>
        do { literal "false"; return $ TBool False }

charToken :: MParser Token
charToken = do
    literalChar '\''
    c <- oneChar
    literalChar '\''
    return $ TChar c

intToken :: MParser Token
intToken = do
        do { n <- naturalNumber; return $ TInt n }
    <|>
        do { literalChar '-'; n <- naturalNumber; return $ TInt (-n) }
    <|>
        do { literalChar '+'; n <- naturalNumber; return $ TInt n }

stringToken :: MParser Token
stringToken = do
    literalChar '"'
    s <- oneChar `while` (/= '"')
    literalChar '"'
    return $ TString s

identifierToken :: MParser Token
identifierToken = do
    name <- identifier
    return $ TIdentifier name

keywordToken :: String -> MParser Token
keywordToken s = do
    lit <- literal s
    spaces
    return $ TKeyword lit

operatorToken :: String -> MParser Token
operatorToken s = do
    lit <- literal s
    spaces
    return $ TOperator lit

lparToken :: MParser Token
lparToken = do
    literalChar '('
    return $ TLPar

rparToken :: MParser Token
rparToken = do
    literalChar ')'
    return $ TRPar
