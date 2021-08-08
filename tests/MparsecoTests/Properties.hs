{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module MparsecoTests.Properties
(
    prop1,
    prop_seq_1,
    prop_seq_2,
    prop_seq_3,
    prop_seq_4,
    prop_alt_1,
    prop_alt_2,
    prop_alt_3,
    prop_alt_4,
    prop_alt_seq_1,
    prop_alt_seq_2,
    prop_alt_seq_3,
    prop_alt_seq_4,
    prop_filt_alt_1,
    prop_filt_alt_2,
    prop_bchoice_seq_1,
    prop_tok_untok_1
)
where

import Mparseco
import MparsecoTests.Generators
import Test.QuickCheck

-- Variable naming conventions:
-- Name     Description         Type
-- g        Generator           Gen State
-- s        State/String        State
-- p,q,r    Parser              MParser a b
-- k,h      Continuation        b -> MParser a b
-- f        Function/Predicate  a -> b, a -> Bool


-- prop0' g v = forAll g $ \s -> () == (parse (return c) s)



-- Property 1:
--   For any string s of type g, if s is non-empty, parsing one character of s results in a pair with the character and the remainder, and no other options.
prop1 g = forAll g $ \s -> s /= "" ==> parse oneChar s == [(head s, tail s)]


-- Sequence laws

-- Sequencing Property 1:
--   'parse' is homomorphic for sequencing.
--   Parsing a string s with a parser sequence (p >>= k) is the same as parsing s with parser p
--   yielding output a and remainder y and then parsing the remainder y with the parser (k a)
prop_seq_1 g p k = forAll g $ \s -> (parse p s >>= \(a,y) -> parse (k a) y) == (parse (p >>= k) s)

-- Sequencing Property 2:
--   The result of first applying the parser that on any input s returns [(a,s)]
--   and then passing this to a the continuation k, is the same as invoking k on a.
-- prop_seq_2 g a k = forAll g $ \s -> parse (do { b <- return a; k b }) s == parse (k a) s
-- prop_seq_2 g a k = forAll g $ \s -> parse (return a >>= k) s == parse (k a) s
prop_seq_2 g a k = forAll g $ (return a >>= k) `parserEq` k a

-- Sequencing Property 3:
--   The result of first applying a parser p and then returning its result
--   is the same as just applying the parser p.
-- prop_seq_3 g p = forAll g $ \s -> parse (do { a <- p; return a }) s == parse p s
-- prop_seq_3 g p = forAll g $ \s -> parse (p >>= return) s == parse p s
-- prop_seq_3 g p = forAll g $ \s -> parse (p >>= \a -> return a) s == parse p s
-- prop_seq_3 g p = forAll g $ \s -> p >>= \a -> return a `parserEq` p
prop_seq_3 g p = forAll g $ (p >>= return) `parserEq` p

-- Sequencing Property 4:
--   Sequencing is associative.
prop_seq_4 g p k h =
    forAll g $
        do { a <- p; do { b <- k a; h b }} `parserEq` do { b <- do { a <- p; k a }; h b }
-- prop_seq_4 g p k h =
--     forAll g $
--         (p >>= \a -> (k a >>= \b -> h b)) `parserEq` ((p >>= \a -> k a) >>= \b -> h b)


-- Alternation laws

-- Alternation Property 1:
--   'parse' is homomorphic for alternation.
--   Parsing a string s with the parser (p <|> q) is the same as the alternative between
--   parsing s with p and parsing s with q.
prop_alt_1 g p q = forAll g $ \s -> ((parse p s) <|> (parse q s)) == (parse (p <|> q) s)

-- Alternation Property 2:
--   The parser that always fails is the left identity for alternation.
-- prop_alt_2 g p = forAll g $ \s -> parse (empty <|> p) s == parse p s
prop_alt_2 g p = forAll g $ (empty <|> p) `parserEq` p

-- Alternation Property 3:
--   The parser that always fails is the right identity for alternation.
-- prop_alt_3 g p = forAll g $ \s -> parse (p <|> empty) s == parse p s
prop_alt_3 g p = forAll g $ (p <|> empty) `parserEq` p

-- Alternation Property 4:
--   Alternation is associative.
prop_alt_4 g p q r = forAll g $ (p <|> (q <|> r)) `parserEq` ((p <|> q) <|> r)


-- Laws linking sequencing and alternation

-- Seq/Alt Property 1:
--   The identity of alternation is a zero of sequencing on the left.
--   Sequencing preserves the identity of alternation.
prop_alt_seq_1 g k = forAll g $ (empty >>= k) `parserEq` empty

-- Seq/Alt Property 2:
--   The identity of alternation is a zero of sequencing on the right.
-- prop_alt_seq_2 g p = forAll g $ (p >>= \a -> empty) `parserEq` empty
prop_alt_seq_2 g p = forAll g $ (p >>= discardAndThenEmpty) `parserEq` empty
    where
        discardAndThenEmpty :: (Eq a, Eq b) => b -> MParser a b
        discardAndThenEmpty a = empty
-- prop_alt_seq_2 g p = forAll g $ (p >>= \(a :: Eq a => a) -> empty) `parserEq` empty
-- prop_alt_seq_2 :: Eq b => Gen State -> MParser b -> Property
-- prop_alt_seq_2 g p = forAll g $ (p >>= \a -> (empty :: Eq a => MParser a)) `parserEq` empty
-- prop_alt_seq_2 g p = forAll g $ (p >>= \a -> empty) `parserEq` (empty :: Eq a => MParser a)
-- prop_alt_seq_2 g p = forAll g $ (p >>= ((\a -> empty) :: Eq a => a -> MParser a)) `parserEq` empty
-- prop_alt_seq_2 g p = forAll g $ do { a <- p; empty } `parserEq` empty

-- Seq/Alt Property 3:
--   Sequencing distributes leftward over alternation.
prop_alt_seq_3 g p q k = forAll g $ ((p <|> q) >>= k) `parserEq` ((p >>= k) <|> (q >>= k))

-- Seq/Alt Property 4:
--   Sequencing distributes rightward over alternation if the parsers are unambiguous.
prop_alt_seq_4 g p k h = forAll g $ (p >>= \a -> ((k a) <|> (h a))) `parserEq` ((p >>= k) <|> (p >>= h))


-- Filtering laws

-- Filtering Property 1:
--   The identity of alternation is a zero of filtering on the left.
--   Filtering preserves the identity of alternation.
prop_filt_alt_1 g f = forAll g $ (empty |> f) `parserEq` empty

-- Filtering Property 2:
--   Filtering distributes leftward over alternation.
prop_filt_alt_2 g p q f = forAll g $ ((p <|> q) |> f) `parserEq` ((p |> f) <|> (q |> f))


-- Biased choice laws

-- Biased choice Property 1:
--   Sequencing distributes rightward over biased choice if the parsers are unambiguous.
prop_bchoice_seq_1 g p k h = forAll g $ ((p >>= k) </> (p >>= h)) `parserEq` (p >>= \a -> ((k a) </> (h a)))


-- Tokenizing properties

-- Tokenizing Property 1:
--   tokenize is left inverse of untokenize. This is, the result of untokenizing a list of tokens and then
--   tokenizing that string results in the original list of tokens with no left-over string.
prop_tok_untok_1 =
    forAll arbitraryTokens $
        \tokens ->
            tokens /= []
            ==>
            let
                kwds = map keywordTokenString $ filter isKeywordToken tokens
                ops = map operatorTokenString $ filter isOperatorToken tokens
            in
                tokenize kwds ops (untokenize tokens) == [(tokens,"")]
