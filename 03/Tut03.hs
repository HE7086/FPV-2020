module Tut03 where

import Data.List (transpose, sort)

{-T3.2-}
dimensions :: [[a]] -> (Int,Int)
dimensions [] = (0,0)
dimensions xs@(x:xt) 
  | and [length x == length y | y <- xt] = (length xs, length x)
  | otherwise = (-1, -1)

isSquare :: [[a]] -> Bool
isSquare xs = let (x, y) = dimensions xs in
                  x == y && x /= -1

canAdd :: [[a]] -> [[a]] -> Bool
canAdd xs ys = dimensions xs /= (-1, -1) && dimensions xs == dimensions ys

canMult :: [[a]] -> [[a]] -> Bool
canMult xs ys = let (_, x) = dimensions xs
                    (y, _) = dimensions ys
                 in x /= -1 && x == y

diagonal :: [[a]] -> [a]
diagonal m = [m !! i !! i | i <- [0 .. length m - 1]]

cubeDiagonal :: [[[a]]] -> [a]
cubeDiagonal = diagonal . diagonal

matrixAdd :: [[Integer]] -> [[Integer]] -> [[Integer]]
matrixAdd a b 
  | canAdd a b = [[x | x <- zipWith (+) aRow bRow] | (aRow,bRow) <- zip a b]
  | otherwise = []

matrixMult :: [[Integer]] -> [[Integer]] -> [[Integer]]
matrixMult a b
  | canMult a b = let bT = transpose b in
    [[sum [x | x <- zipWith (*) aRow bCol] | bCol <- bT ] | aRow <- a] 
  | otherwise = []

--zip = zipWith (,)
{-T3.3-}

splitList :: [a] -> ([a],[a])
splitList xs = splitAt n xs
  where n = length xs `div` 2

mergeLists :: ([Integer],[Integer]) -> [Integer]
mergeLists ([],bs) = bs
mergeLists (as,[]) = as
mergeLists (a:as,b:bs) 
  | a < b = a : mergeLists (as, b:bs)
  | otherwise = b : mergeLists (a:as, bs)

mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs = 
  let (as, bs) = splitList xs
      cs = mergeSort as
      ds = mergeSort bs
   in mergeLists (cs, ds)

adjacentPairs :: [a] -> [(a,a)]
adjacentPairs xs = zip xs (tail xs)
-- zip [] _ = [] -> lazy

prop_mergeSort :: [Integer] -> Bool
prop_mergeSort xs = sort xs == mergeSort xs
--prop_ms xs = and [x <= y | (x,y) <- adjacentPairs (mergeSort xs)]
{-T4.4-}
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n | n > 1 = n : collatz (if even n then n `div` 2 else 3 * n + 1)

prop_collatz :: Integer -> Bool
prop_collatz n = 
  let xs = collatz n
   in not(null xs) && last xs == 1

prop_collatzOneHundred :: Bool
prop_collatzOneHundred = -- and [prop_collatz n | n <- [1..100]]
  all prop_collatz [1..100]
