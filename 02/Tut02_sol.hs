module Tut02 where

import Data.List (nub)
import qualified Data.List as D (union)
import Test.QuickCheck ((==>), Property)

{-T2.1-}
allSums :: [Integer] -> [Integer] -> [Integer]
allSums xs ys = [x + y | x <- xs, y <- ys]

evens :: [Integer] -> [Integer]
evens xs = [x | x <- xs, even x]

nLists :: [Integer] -> [[Integer]]
nLists xs = [[1..x] | x <- xs]

allEvenSumLists :: [Integer] -> [Integer] -> [[Integer]]
allEvenSumLists xs ys = [[1..x+y] | x <- xs, y <- ys, even (x + y)]

prop_allEvenSumLists :: [Integer] -> [Integer] -> Bool
prop_allEvenSumLists xs ys =
  allEvenSumLists xs ys == nLists (evens (allSums xs ys))

{-T2.2.1-}
toSet :: [Integer] -> [Integer]
toSet = nub

{-T2.2.2-}
isSet :: [Integer] -> Bool
isSet xs = length (toSet xs) == length xs

prop_toSet :: [Integer] -> Bool
prop_toSet xs = isSet $ toSet xs

{-T2.2.3-}
union :: [Integer] -> [Integer] -> [Integer]
-- union xs ys = toSet $ xs ++ ys
union = D.union

prop_unionIsSet :: [Integer] -> [Integer] -> Bool
prop_unionIsSet s t = isSet $ s `union` t

prop_unionSpec1 :: [Integer] -> [Integer] -> Integer -> Property
prop_unionSpec1 s t a = a `elem` s || a `elem` t ==> a `elem` (s `union` t)

prop_unionSpec2 :: [Integer] -> [Integer] -> Integer -> Property
prop_unionSpec2 s t a = a `elem` (s `union` t) ==> a `elem` s || a `elem` t

{-T2.2.4-}
intersection :: [Integer] -> [Integer] -> [Integer]
intersection xs ys = toSet [x | x <- xs, y <- ys, x == y]

prop_intersectionIsSet :: [Integer] -> [Integer] -> Bool
prop_intersectionIsSet s t = isSet $ s `intersection` t

prop_intersectionSpec1 :: [Integer] -> [Integer] -> Integer -> Property
prop_intersectionSpec1 s t a = a `elem` s ==> a `elem` t ==> a `elem` (s `intersection` t)

prop_intersectionSpec2 :: [Integer] -> [Integer] -> Integer -> Property
prop_intersectionSpec2 s t a = a `elem` (s `intersection` t) ==> a `elem` s && a `elem` t

{-T2.2.5-}
diff :: [Integer] -> [Integer] -> [Integer]
diff xs ys = toSet [x | x <- xs, x `notElem` ys]

-- you can do the props on your own by now ;)

{-T2.3-}
eqFrac :: (Integer,Integer) -> (Integer,Integer) -> Bool
eqFrac (a,b) (c,d) = a*d == c * b

prop_eqFracRefl :: Integer -> Integer -> Bool
prop_eqFracRefl a b = eqFrac (a, b) (a, b)
prop_eqFracSymm :: Integer -> Integer -> Integer -> Integer -> Property
prop_eqFracSymm a b c d = eqFrac (a, b) (c, d) ==> eqFrac (c, d) (a, b)
prop_eqFracScale :: Integer -> Integer -> Integer -> Bool
prop_eqFracScale a b n = eqFrac (a, b) (n * a, n * b)

{-T2.4-}
pow2_slow :: Integer -> Integer
pow2_slow 0 = 1
pow2_slow n | n > 0 = 2 * pow2_slow (n - 1)

pow2 :: Integer -> Integer
pow2 0 = 1
pow2 n
  | n < 0 = undefined
  | even n = let k = pow2 (n `div` 2) in k * k
  | otherwise = 2 * pow2 (n - 1)
