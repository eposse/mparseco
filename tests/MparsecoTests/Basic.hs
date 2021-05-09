module MparsecoTests.Basic
(
    prop1
)
where

import Mparseco
import Test.QuickCheck

prop1 g = forAll g $ \s -> case s of {[] -> True; (c:cs) -> parse oneChar s == [(c,cs)]}
