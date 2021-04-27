module Mparseco.Core
(
    State,
    MParser (..),
    parse,
    empty,
    (<|>),
    (|>),
    allZeroOrMore,
    maxZeroOrMore
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
        (p >>= \a -> allZeroOrMore p >>= \x -> return (a:x))
    <|>
        return []

-- | Biased choice
(</>) :: Eq a => MParser a -> MParser a -> MParser a
p1 </> p2 = MParser (\s -> let left = parse p1 s in if left /= [] then left else parse p2 s)

-- | Greedy parser iteration
maxZeroOrMore :: Eq a => MParser a -> MParser [a]
maxZeroOrMore p =
        (p >>= \a -> maxZeroOrMore p >>= \x -> return (a:x))
    </>
        return []
