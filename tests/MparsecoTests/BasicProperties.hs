{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module MparsecoTests.BasicProperties
(
    prop1,
    prop2,
    prop3,
    prop4,
    prop5,
    prop6,
    prop7,
    prop8,
    prop9
)
where

import Mparseco
import Test.QuickCheck

-- Variable naming conventions:
-- Name     Description     Type
-- g        Generator       Gen State
-- s        State/String    State
-- p,q,r    Parser          MParser a
-- k        continuation    a -> MParser a

-- Property 1:
--   For any string s of type g, if s is non-empty, parsing one character of s results in a pair with the character and the remainder, and no other options.
prop1 g = forAll g $ \s -> s /= "" ==> parse oneChar s == [(head s, tail s)]


-- Sequence laws

-- Property 2:
--   The result of first applying the parser that on any input s returns [(a,s)]
--   and then passing this to a the continuation k, is the same as invoking k on a.
-- prop2 g a k = forAll g $ \s -> parse (do { b <- return a; k b }) s == parse (k a) s
-- prop2 g a k = forAll g $ \s -> parse (return a >>= k) s == parse (k a) s
prop2 g a k = forAll g $ (return a >>= k) `parserEq` k a

-- Property 3:
--   The result of first applying a parser p and then returning its result
--   is the same as just applying the parser p.
-- prop3 g p = forAll g $ \s -> parse (do { a <- p; return a }) s == parse p s
-- prop3 g p = forAll g $ \s -> parse (p >>= return) s == parse p s
-- prop3 g p = forAll g $ \s -> parse (p >>= \a -> return a) s == parse p s
-- prop3 g p = forAll g $ \s -> p >>= \a -> return a `parserEq` p
prop3 g p = forAll g $ (p >>= return) `parserEq` p

-- Property 4:
--   Sequencing is associative.
prop4 g p1 p2 p3 =
    forAll g $
        do { a <- p1; do { b <- p2 a; p3 b }} `parserEq` do { b <- do { a <- p1; p2 a }; p3 b }
-- prop4 g p1 p2 p3 =
--     forAll g $
--         (p1 >>= \a -> (p2 a >>= \b -> p3 b)) `parserEq` ((p1 >>= \a -> p2 a) >>= \b -> p3 b)


-- Alternation laws

-- Property 5:
--   The parser that always fails is the left identity for alternation.
-- prop5 g p = forAll g $ \s -> parse (empty <|> p) s == parse p s
prop5 g p = forAll g $ (empty <|> p) `parserEq` p

-- Property 6:
--   The parser that always fails is the right identity for alternation.
-- prop6 g p = forAll g $ \s -> parse (p <|> empty) s == parse p s
prop6 g p = forAll g $ (p <|> empty) `parserEq` p

-- Property 7:
--   Alternation is associative.
prop7 g p q r = forAll g $ (p <|> (q <|> r)) `parserEq` ((p <|> q) <|> r)

-- Property 8:
--   The identity of alternation is a zero of sequencing on the left.
prop8 g k = forAll g $ (empty >>= k) `parserEq` empty

-- Property 9:
--   The identity of alternation is a zero of sequencing on the right.
-- prop9 g p = forAll g $ (p >>= \a -> empty) `parserEq` empty
prop9 g p = forAll g $ (p >>= discardAndThenEmpty) `parserEq` empty
    where
        discardAndThenEmpty :: Eq a => a -> MParser a
        discardAndThenEmpty a = empty
-- prop9 g p = forAll g $ (p >>= \(a :: Eq a => a) -> empty) `parserEq` empty
-- prop9 :: Eq b => Gen State -> MParser b -> Property
-- prop9 g p = forAll g $ (p >>= \a -> (empty :: Eq a => MParser a)) `parserEq` empty
-- prop9 g p = forAll g $ (p >>= \a -> empty) `parserEq` (empty :: Eq a => MParser a)
-- prop9 g p = forAll g $ (p >>= ((\a -> empty) :: Eq a => a -> MParser a)) `parserEq` empty
-- prop9 g p = forAll g $ do { a <- p; empty } `parserEq` empty
