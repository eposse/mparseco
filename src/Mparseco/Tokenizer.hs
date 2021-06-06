module Mparseco.Tokenizer
(
    Token(..),
    next,
    nextToken,
    nextToken',
    basicToken,
    token,
    tokenizer,
    tokenizer',
    tokenize,
    tokenize',
    boolToken,
    charToken,
    intToken,
    stringToken,
    identifierToken,
    literalToken,
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
    | TLiteral String
    | TIdentifier String
    | TLPar
    | TRPar
    deriving (Eq, Show)

next :: MParser Token -> MParser Token
next token = do
    spaces
    token

nextToken :: MParser Token
nextToken = next basicToken

nextToken' :: [String] -> MParser Token
nextToken' kwds = next $ token kwds

basicToken :: MParser Token
basicToken = oneof [boolToken, charToken, intToken, stringToken, identifierToken]

token :: [String] -> MParser Token
token kwds = do
    (oneof [literalToken k | k <- kwds])
    <|>
    basicToken

tokenizer :: MParser [Token]
tokenizer = maxZeroOrMore nextToken

tokenizer' :: [String] -> MParser [Token]
tokenizer' kwds = maxZeroOrMore $ nextToken' kwds

tokenize :: String -> [([Token], State)]
tokenize = parse tokenizer

tokenize' :: [String] -> String -> [([Token], State)]
tokenize' kwds = parse $ tokenizer' kwds

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

literalToken :: String -> MParser Token
literalToken s = do
    lit <- literal s
    return $ TLiteral lit

lparToken :: MParser Token
lparToken = do
    literalChar '('
    return $ TLPar

rparToken :: MParser Token
rparToken = do
    literalChar ')'
    return $ TRPar
