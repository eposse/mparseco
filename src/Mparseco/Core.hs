module Mparseco.Core
(
    MParser (..),
    StringParser (..),
    parse,
    parserEq,
    empty,
    (<|>),
    (|>),
    (</>),
    allZeroOrMore,
    allOneOrMore,
    maxZeroOrMore,
    maxOneOrMore,
    while,
    anyof,
    oneof
)
where

import Control.Applicative (Applicative(..),Alternative(..))
import Control.Monad       (MonadPlus(..), liftM, ap)
import Debug.Trace

-- | MParser a b is the type of parsers that receive input of type a and produce output of type b
newtype MParser a b = MParser (a -> [(b,a)])

type StringParser = MParser String

instance Show (MParser a b) where
    show (MParser f) = "MParser <...>"

parse :: MParser a b -> a -> [(b,a)]
-- parse p s | trace ("parse (" ++ show p ++ ") " ++ show s) False = undefined
parse (MParser f) s = f s

parserEq :: (Eq a, Eq b) => MParser a b -> MParser a b -> a -> Bool
parserEq p q = \s -> parse p s == parse q s

instance Functor (MParser a) where
    fmap = liftM

instance Applicative (MParser a) where
    pure  = return
    (<*>) = ap

-- | Parser sequencing
-- Interesting: note that this is almost the same as the State monad,
-- but producing a list of value/state pairs rather than a single one.
-- Perhaps this can be interpreted as thinking that parsers are non-deterministic state machines.
instance Monad (MParser a) where
    return c = MParser (\s -> [(c,s)])
    p >>= k  = MParser (\s -> [(b,z) | (a,y) <- parse p s, (b,z) <- parse (k a) y])

-- | Parser alternation
instance Alternative (MParser a) where
    empty = MParser (\s -> [])
    -- p1 <|> p2 | trace ("(<|>) (" ++ show p1 ++ ") (" ++ show p2 ++ ")") False = undefined
    p1 <|> p2 = MParser (\s -> (parse p1 s) ++ (parse p2 s))

instance MonadPlus (MParser a) where
    mzero = empty
    mplus = (<|>)

-- | Parser failure
instance MonadFail (MParser a) where
    fail _ = empty

-- | Parser filetering
(|>) :: MParser a b -> (b -> Bool) -> MParser a b
p |> f = p >>= \a -> if f a then return a else empty

-- | Parser iteration
allZeroOrMore :: MParser a b -> MParser a [b]
allZeroOrMore p =
        do { a <- p; as <- allZeroOrMore p; return (a:as) }
    <|>
        return []

allOneOrMore :: MParser a b -> MParser a [b]
allOneOrMore p =
    do
        a <- p
        as <- allZeroOrMore p
        return (a:as)

-- | Biased choice
(</>) :: (Eq a, Eq b) => MParser a b -> MParser a b -> MParser a b
p1 </> p2 = MParser (\s -> let left = parse p1 s in if left /= [] then left else parse p2 s)

-- | Greedy parser iteration
maxZeroOrMore :: (Eq a, Eq b) => MParser a b -> MParser a [b]
maxZeroOrMore p =
        do { a <- p; as <- maxZeroOrMore p; return (a:as) }
    </>
        return []

maxOneOrMore :: (Eq a, Eq b) => MParser a b -> MParser a [b]
maxOneOrMore p =
    do
        a <- p
        as <- maxZeroOrMore p
        return (a:as)

-- | Greedy parser iteration with continuation condition
while :: (Eq a, Eq b) => MParser a b -> (b -> Bool) -> MParser a [b]
while p f =
        do
            a <- p |> f
            x <- while p f
            return (a:x)
    </>
        return []

-- | Parser selection
anyof :: [MParser a b] -> MParser a b
anyof [] = empty
anyof (p:ps) = p <|> (anyof ps)
-- anyof = foldl (<|>) empty

oneof :: (Eq a, Eq b) => [MParser a b] -> MParser a b
oneof [] = empty
oneof (p:ps) = p </> (oneof ps)
