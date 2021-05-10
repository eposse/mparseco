module MparsecoTests.BasicProperties
(
    prop1,
    prop2,
    prop3
)
where

import Mparseco
import Test.QuickCheck

-- Property 1:
--   For any string s of type g, if s is non-empty, parsing one character c of s results in c and the remainder (cs).
prop1 g = forAll g $ \s -> case s of {[] -> True; (c:cs) -> parse oneChar s == [(c,cs)]}

-- Alternation laws

-- Property 2:
--   The parser that always fails is the left identity for alternation.
prop2 g p = forAll g $ \s -> parse (empty <|> p) s == parse p s

-- Property 3:
--   The parser that always fails is the right identity for alternation.
prop3 g p = forAll g $ \s -> parse (p <|> empty) s == parse p s
