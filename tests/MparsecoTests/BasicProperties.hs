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
    prop9,
    prop10,
    prop11,
    prop12,
    prop13,
    prop14
)
where

import Mparseco
import Test.QuickCheck

-- Variable naming conventions:
-- Name     Description         Type
-- g        Generator           Gen State
-- s        State/String        State
-- p,q,r    Parser              MParser a
-- k,h      Continuation        a -> MParser a
-- f        Function/Predicate  a -> b, a -> Bool

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
prop4 g p k h =
    forAll g $
        do { a <- p; do { b <- k a; h b }} `parserEq` do { b <- do { a <- p; k a }; h b }
-- prop4 g p k h =
--     forAll g $
--         (p >>= \a -> (k a >>= \b -> h b)) `parserEq` ((p >>= \a -> k a) >>= \b -> h b)


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
--   Sequencing preserves the identity of alternation.
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

-- Property 10:
--   Sequencing distributes leftward over alternation.
prop10 g p q k = forAll g $ ((p <|> q) >>= k) `parserEq` ((p >>= k) <|> (q >>= k))

-- Property 11:
--   Sequencing distributes rightward over alternation if the parsers are unambiguous.
prop11 g p k h = forAll g $ (p >>= \a -> ((k a) <|> (h a))) `parserEq` ((p >>= k) <|> (p >>= h))


-- Filtering laws

-- Property 12:
--   The identity of alternation is a zero of filtering on the left.
--   Filtering preserves the identity of alternation.
prop12 g f = forAll g $ (empty |> f) `parserEq` empty

-- Property 13:
--   Filtering distributes leftward over alternation.
prop13 g p q f = forAll g $ ((p <|> q) |> f) `parserEq` ((p |> f) <|> (q |> f))


-- Biased choice laws

-- Property 14:
--   Sequencing distributes rightward over biased choice if the parsers are unambiguous.
prop14 g p k h = forAll g $ ((p >>= k) </> (p >>= h)) `parserEq` (p >>= \a -> ((k a) </> (h a)))
