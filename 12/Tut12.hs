module Tut12 where

{-
map _ [] = []
map f ( x : xs ) = f x : map f xs

( x : xs ) !! n 
    | n == 0 = x
    | otherwise = xs !! ( n - 1)

threes = 3 : threes

 - (map (*2) (1 : threes)) !! 1

 - map (*2) (1 : threes) !! 1
 = (*2) 1 : map (*2) threes !! 1
 = map (*2) threes !! (1 - 1)
 = map (*2) (3 : threes) !! (1 - 1)
 = (*2) 3 : map (*2) threes !! (1 - 1)
 = (*2) 3 : map (*2) threes !! 0
 = (*2) 3
 = 6


 - (\f -> (\x -> x + f 2 + x)) (\y -> y * 2) (3 + 1)
 = (\x -> x + (\y -> y * 2) 2 + x) (3 + 1)
 = (3 + 1) + (\y -> y * 2) 2 + (3 + 1)
 = 4 + (\y -> y * 2) 2 + 4 -- sharing
 = 4 + 2 * 2 + 4
 = 4 + 4 + 4
 = 8 + 4
 = 12

filter _ [] = []
filter f ( x : xs ) 
    | f x = x : filter f xs
    | otherwise = filter f xs

head (x : xs) = x

 - head (filter (/=3) threes)
 - head (filter (/=3) (3 : threes))
 - head (filter (/=3) threes)
 - head (filter (/=3) (3 : threes))

 --... not terminate


inf = inf
inf == inf
inf == inf
...

-}


fibs1 :: [Integer]
fibs1 = 0 : 1 : zipWith (+) fibs1 (tail fibs1)

-- time    : O(n)
-- storage : O(n)

-- fibs     : 0 1 1 2 3 ..
-- tail fibs: 1 1 2 3 ..


fibs2 :: [Integer]
fibs2 = map fib [0..]
    where
        fib 0 = 0
        fib 1 = 1
        fib n = fibs2 !! (n - 1) + fibs2 !! (n - 2)

-- (!!) -> O(n)
-- time : O(n^2) of (!!), O(n) of addition

fib3 :: Integer -> Integer
fib3 0 = 0
fib3 1 = 1
fib3 n = fib3 (n - 1) + fib3 (n - 2)
-- time: O(2^n)

{-==========Note to sharing==========
sharing in the actual compiler does not always do the same, it really depends on the context.
to find out what the compiler actually does, run `ghci -ddump-simpl`

In our (simplified) case, sharing only happens when 2 expressions have the exact same name,
or can be represented by a single `where` or `let` clause

--------------------

e.g. 
f x = 2 * x + 2 * x

that is the same as

f x = y + y
    where y = 2 * x

or

f x = let y = 2 * x in y + y

so that `2 * x` is only calculated once

--------------------

e.g. in fibs1:

fibs1 = 0 : 1 : zipWith (+) __SHARE__ (tail __SHARE__)
    where __SHARE__ = fibs1

fibs1 is shared here 

--------------------
> but why it is not happening in fib3?
> can't fib3 also be replaced by SHARE?

no, fib3 itself is only a reference of (not yet evaluated) function, 
even if it is called twice, its parameters are different.
so the sharing is not working.

further reading:
https://www.well-typed.com/blog/2016/09/sharing-conduit/
(warning: advanced stuff)
-}

{-  an O(n log n) implemtation of fib3 using custom binary trees

-- data Tree a = Tree (Tree a) a (Tree a)

-- instance Functor Tree where
--   fmap f (Tree l v r) = Tree (fmap f l) (f v) (fmap f r)

-- infixl 9 !!!

-- (!!!) :: Tree a -> Integer -> a
-- (Tree _ v _) !!! 0 = v
-- (Tree l _ r) !!! n = case (n - 1) `divMod` 2 of
--   (q, 0) -> l !!! q
--   (q, 1) -> r !!! q
-- nats :: Tree Integer
-- nats = go 0 1
--   where
--     go n s = Tree (go l s') n (go r s')
--        where
--          l = n + s
--          r = l + s
--          s' = s * 2

-- fibs3 :: Tree Integer
-- fibs3 = fmap fib_fast nats
--   where
--     fib_fast 0 = 0
--     fib_fast 1 = 1
--     fib_fast n = fibs3 !!! (n-1) + fibs3 !!! (n-2)
-}

zipWithN :: ([a] -> b) -> [[a]] -> [b]
zipWithN f xs
  | null xs || any null xs = []
  | otherwise = f (map head xs) : zipWithN f (map tail xs)

nonaccis :: Int -> [Integer]
nonaccis n = replicate (n - 1) 0 ++ [1] ++ nonaccis'
  where
    iterateN x f = take x . iterate f
    nonaccis' = zipWithN sum $ iterateN n tail (nonaccis n)

nonaccis2 :: Int -> [Integer]
nonaccis2 n = map f [0..]
  where
    f x 
      | x < n - 1 = 0
      | x == n - 1 = 1
      | otherwise = sum $ take n $ drop (x - n) (nonaccis2 n)


