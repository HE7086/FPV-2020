module Tut06 where

import Test.QuickCheck ( (==>), Property )

length' :: [a] -> Integer
-- length' xs = let f a x = x + 1 in foldr f 0 xs
-- length' xs = foldr f 0 xs
--   where 
--     f :: a -> Integer -> Integer
--     f a x = x + 1
--length' xs = foldr (\_ x -> x + 1) 0 xs
--length' xs = foldr (const (+1)) 0 xs
length' = foldr (const succ) 0

-- length' [a,b,c]
--   = foldr f 0 [a, b, c]
--   = f a (foldr f 0 [b, c])
--   = f a (f b (foldr f 0 [c]))
--   = f a (f b (f c 0))
--   = f a (f b 1)
--   = f a 2
--   = 3

map' :: (a -> b) -> [a] -> [b]
map' f xs = let g a b = f a : b in foldr g [] xs
-- map' f xs = foldr (\a b -> f a : b) [] xs
-- map' f xs = foldr ((:) . f) [] xs

-- map' f [a,b,c]
--   = foldr g [] [a, b, c]
--   = f a : (foldr g [] [b, c])
--   = f a : (f b : (foldr f 0 [c]))
--   = f a : (f b : (f c : []))
--   = f a : (f b : [f c])
--   = f a : [f b, f c]
--   = [f a, f b, f c]

reverse' :: [a] -> [a]
--reverse' xs = let f x y = (y ++ [x]) in foldr f [] xs
-- reverse' xs = foldr (\x y -> (y ++ [x])) [] xs
--reverse' = foldr (flip (++) . (:[])) []
reverse' = foldr (flip mappend . return) []

minimum' :: Ord a => a -> [a] -> a
minimum' = foldr min

fib :: Integer -> Integer
fib n = fst $ foldr (\_ (prev1, prev2) -> (prev2, prev1 + prev2)) (0,1) [1..n]
--fib n = unfoldr (\(f, s) -> Just (f, (s, f + s))) (0,1) `genericIndex` n

inits' :: [a] -> [[a]]
inits' xs = snd $ foldr (\_ (xsInner,res) -> (init xsInner, xsInner:res)) (xs,[]) [0..length xs]
--inits' xs = foldr (\i ys -> take i xs : ys) [] [0 .. length xs]
-- [1,2,3]
-- [[1,2,3], [1,2], [1], []]

squareOn :: (Eq a, Num a) => [a] -> a -> a
squareOn = foldr (\x acc y -> if y == x then x*x else acc y) id
-- squareOn xs x = if x `elem` xs then x*x else x

compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- compose [f, g, h]
--   = f . g . h . id

{-T6.2-}
iter :: Int -> (a -> a) -> a -> a
iter n f x
    | n <= 0 = x
    | otherwise = iter (n - 1) f (f x)

-- iter 3 f x = f (f (f x))
-- f $ f x
-- (f . f) x
-- iter n f x = take n $ iterate f x

pow :: Int -> Int -> Int
pow n k = iter k (n *) 1

drop' :: Int -> [a] -> [a]
--drop' n ys = iter n (\(x:xs) -> xs) ys
drop' n = iter n tail

replicate' :: Int -> a -> [a]
replicate' n x = iter n (x:) []

iterWhile :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterWhile test f x = if test x (f x)
                        then iterWhile test f (f x)
                        else x

findSup :: Ord a => (a -> a) -> a -> a -> a
findSup f m = iterWhile (const (<= m)) f
-- test x (f x)
-- = (const (<= m)) x (f x)
-- = (<= m) (f x)
-- = f x <= m

{-T6.3-}
-- data Mydata a = Mydata a a -- lazy
-- newtype Mytype a = Mytype a -- strict

type ChurchNum a = (a -> a) -> a -> a -- alias

zero :: ChurchNum a
zero _ x = x

one :: ChurchNum a
one f = f
-- one = ($) 

two :: ChurchNum a
two f x = f (f x)

fromInt :: Int -> ChurchNum a
fromInt = iter

toInt :: ChurchNum Int -> Int
toInt n = n (+1) 0

prop_fromTo :: Int -> Property
prop_fromTo x = x >= 0 ==> toInt (fromInt x) == x

succ' :: ChurchNum a -> ChurchNum a
succ' n f x = f $ n f x

plus :: ChurchNum a -> ChurchNum a -> ChurchNum a
plus n m f x = n f $ m f x

mult :: ChurchNum a -> ChurchNum a -> ChurchNum a
mult n m f x = n $ m f x

prop_mult :: Int -> Int -> Property
prop_mult x y = min x y >= 0 ==> x * y == toInt (fromInt x `mult` fromInt y)
