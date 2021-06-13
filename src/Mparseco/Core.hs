module Mparseco.Core
(
    State,
    MParser (..),
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

type State = String
newtype MParser a = MParser (State -> [(a,State)])

instance Show (MParser a) where
    show (MParser f) = "MParser <...>"

parse :: MParser a -> State -> [(a,State)]
-- parse p s | trace ("parse (" ++ show p ++ ") " ++ show s) False = undefined
parse (MParser f) s = f s

parserEq :: Eq a => MParser a -> MParser a -> State -> Bool
parserEq p q = \s -> parse p s == parse q s

instance Functor MParser where
    fmap = liftM

instance Applicative MParser where
    pure  = return
    (<*>) = ap

-- | Parser sequencing
instance Monad MParser where
    return c = MParser (\s -> [(c,s)])
    p >>= k  = MParser (\s -> [(b,z) | (a,y) <- parse p s, (b,z) <- parse (k a) y])

-- | Parser alternation
instance Alternative MParser where
    empty = MParser (\s -> [])
    -- p1 <|> p2 | trace ("(<|>) (" ++ show p1 ++ ") (" ++ show p2 ++ ")") False = undefined
    p1 <|> p2 = MParser (\s -> (parse p1 s) ++ (parse p2 s))

-- | Parser filetering
(|>) :: MParser a -> (a -> Bool) -> MParser a
p |> f = p >>= \a -> if f a then return a else empty

-- | Parser iteration
allZeroOrMore :: MParser a -> MParser [a]
allZeroOrMore p =
        do { a <- p; as <- allZeroOrMore p; return (a:as) }
    <|>
        return []

allOneOrMore :: MParser a -> MParser [a]
allOneOrMore p =
    do
        a <- p
        as <- allZeroOrMore p
        return (a:as)

-- | Biased choice
(</>) :: Eq a => MParser a -> MParser a -> MParser a
p1 </> p2 = MParser (\s -> let left = parse p1 s in if left /= [] then left else parse p2 s)

-- | Greedy parser iteration
maxZeroOrMore :: Eq a => MParser a -> MParser [a]
maxZeroOrMore p =
        do { a <- p; as <- maxZeroOrMore p; return (a:as) }
    </>
        return []

maxOneOrMore :: Eq a => MParser a -> MParser [a]
maxOneOrMore p =
    do
        a <- p
        as <- maxZeroOrMore p
        return (a:as)

-- | Greedy parser iteration with continuation condition
while :: Eq a => MParser a -> (a -> Bool) -> MParser [a]
while p f =
        do
            a <- p |> f
            x <- while p f
            return (a:x)
    </>
        return []

-- | Parser selection
anyof :: [MParser a] -> MParser a
anyof [] = empty
anyof (p:ps) = p <|> (anyof ps)
-- anyof = foldl (<|>) empty

oneof :: Eq a => [MParser a] -> MParser a
oneof [] = empty
oneof (p:ps) = p </> (oneof ps)
