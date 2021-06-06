module Mparseco.Tokenizer
(
    Token(..),
    next,
    boolToken,
    charToken,
    intToken,
    stringToken,
    -- literalToken,
    -- identifierToken,
    -- lparToken,
    -- rparToken
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
