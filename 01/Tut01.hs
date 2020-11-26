module Tut01 where

import Test.QuickCheck
import Data.List

{-T1.1-}
offByOne :: Integer -> Integer -> Bool
offByOne x y = x + 1 == y || y + 1 == x

threeAscending :: Integer -> Integer -> Integer -> Bool
threeAscending x y z = x < y && y < z

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = a == b && b == c && c == d

{- T1.2 -}
fac :: Integer -> Integer
fac 0 = 0
fac 1 = 1
fac n = n * fac (n - 1)

sumEleven :: Integer -> Integer
sumEleven n = helper n 10
    where
        helper n 0 = n
        helper n m = n + m + helper n (m - 1)

-- sumEleven :: Integer -> Integer
-- sumEleven n = helper n 10

-- helper :: Integer -> Integer -> Integer
-- helper n 0 = n
-- helper n m = n + m + helper n (m - 1)

-- sE :: Integer -> Integer
-- sE n = sum [n .. n + 10]

{- T1.3 -}
g :: Integer -> Integer
g n = if n < 10 then n * n else n

-- argMaxG n = fst $ maximumBy snd $ zip [0 .. n] (map g [0 .. n])

argMaxG :: Integer -> Integer
argMaxG 0 = 0
argMaxG n
  | n > 0 = 
      if g rec_max > g n 
      then rec_max 
      else n
  | otherwise = 0
      where rec_max = argMaxG (n - 1)

argMaxG' :: Integer -> Integer
argMaxG' n
  | n < 0 = 0
  | n >= 10 && n < 81 = 9
  | otherwise = n

prop_argMaxGEquiv :: Integer -> Bool
prop_argMaxGEquiv n = argMaxG n == argMaxG' n
