module Tut13 where

import Prelude hiding (concat)
import Data.List hiding (concat)
import Data.Semigroup

{-T15.1-}

{-

1) tail recursive: the last call is 'prod', all other calculations happen in the
   arguments
2) not tail recursive: after the call to 'prod', there is a multiplication
3) tail recursive: the last call is 'prod', just as in 1). The 'if-then-else'
   is not relevant here because the condition is calculated before the 
   recursive call. After the call no further work needs to be done.

-}

{-T15.2-}

concat :: [[a]] -> [a]
concat = go []
    where go acc [] = acc
          go acc (xs:xss) = go (acc ++ xs) xss

concat' :: [[a]] -> [a]
concat' = go []
    where go acc [] = reverse acc
          go acc (xs:xss) = go (reverse xs ++ acc) xss

{- Why is 'reverse' necessary? Without it, we would need to append to the list (acc ++ xs),
 - which takes quadratic time, because the running time of '++' depends on the length of
 - its first argument.-}

{-T15.4-}
{-
map f (concat xss) = concat (map (map f) xss)
Proof by induction over List xss

Case []
  To show: map f (concat []) = concat (map (map f) [])
    map f (concat [])
    = map f []  -- def concat
    = []        -- def map

    concat (map (map f) [])
    = concat []  -- def map
    = []         -- def concat

Case (xs:xss)
  To show: map f (concat (xs:xss)) = concat (map (map f) (xs:xss))
  IH: map f (concat xss) = concat (map (map f) xss)
    map f (concat (xs:xss))
    = map f (xs ++ concat xss))             -- def concat
    = map f xs ++ map f (concat xss)        -- Lemma map_append
    = map f xs ++ concat (map (map f) xss)  -- IH

    concat (map (map f) (xs:xss))
    = concat (map f xs : map (map f) xss))  -- def map
    = map f xs ++ concat (map (map f) xss)  -- def concat

QED
-}

{-T15.5-}

-- Implementation not needed for solution!
sortP :: (Ord a, Eq b) => [(a,b)] -> [(a,b)]
sortP = undefined

propIncr xs = isIncr (sortP xs)

isIncr (x:y:xs) = fst x <= fst y && propIncr (y:xs)
isIncr _ = True

propSameSet xs = subsetOf xs ys && subsetOf ys xs
  where ys = sortP xs

subsetOf [] _ = True
subsetOf (x:xs) ys = x `elem` ys && subsetOf xs (delete x ys)

{-T15.6-}
-- Typeclasses

data Pair a = Pair a a 
  deriving (Show)

instance Semigroup a => Semigroup (Pair a) where
  Pair a c <> Pair b d = Pair (a <> b) (c <> d)

instance Monoid a => Monoid (Pair a) where
  mempty = Pair mempty mempty

{-T15.7-}

match :: IO ()
match = play 10 $ cycle [1,2]

readNum :: IO Int
readNum = do
    x <- readLn
    if 1 <= x && x <= 5 then
        return x
    else do
        putStrLn "The input must be between 1 and 5."
        readNum

play :: Show a => Int -> [a] -> IO ()
play n (p:ps)= do
    putStrLn $ "Matches: "
        ++ show n ++ ". Player " ++ show p ++ "?"
    m <- readNum
    if n <= m then
        putStrLn $ "Player " ++ show p ++ " wins!"
    else
        play (n - m) ps
