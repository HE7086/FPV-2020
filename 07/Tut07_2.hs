module Tut07 where

import Data.Ratio

{-T7.1-}
data Fraction = Over Integer Integer

norm :: Fraction -> Fraction
norm (Over a b) = (a `div` c) `Over` (b `div` c)
  where c = gcd a b --ggT

instance Num Fraction where
  (a1 `Over` b1) + (a2 `Over` b2) = norm $ (a1*b2 + a2*b1) `Over` (b1 * b2)
  (a1 `Over` b1) - (a2 `Over` b2) = norm $ (a1*b2 - a2*b1) `Over` (b1 * b2)
  (*) (a1 `Over` b1) (a2 `Over` b2) = norm $ (a1 * a2) `Over` (b1 * b2)
  --negate (a `Over` b) = negate a `Over` b
  fromInteger n = n `Over` 1
  abs (a `Over` b) = abs a `Over` abs b
  signum (a `Over` b) = (signum a * signum b) `Over` 1

-- the Eq and Show instances are not very nice;
-- we write our own ones
instance Eq Fraction where
  (a1 `Over` b1) == (a2 `Over` b2) = a1*b2 == a2*b1

instance Show Fraction where
  show (0 `Over` _) = "0"
  show (a `Over` b) = (if (a < 0) /= (b < 0) then "-" else "") ++ show (abs a) ++ "/" ++ show (abs b)

instance Fractional Fraction where
  recip (a `Over` b) = b `Over` a
  fromRational r = numerator r `Over` denominator r

{-T7.2-}
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

sumTree :: Num a => Tree a -> a
sumTree Leaf = 0
sumTree (Node l v r) = v + sumTree l + sumTree r

--  cut 2
--1:     1
--2:  2     3    -> 1 (Node Leaf 1 Leaf)
-------------------
--3: 4 5           2 3

-- cut 1
--  2     -> 2
-- 4 5

-- cut 0
-- 4
--
-- cut 0
-- 5

-- cut 1
--  3     -> 3

cut :: Tree a -> Integer -> Tree a
cut Leaf _ = Leaf
cut (Node l v r) n
  | n <= 0 = Leaf
  | otherwise = Node (cut l (n-1)) v (cut r (n-1))

-- foldr f x []

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree _ x Leaf = x
foldTree f x (Node l a r) = foldTree f (f a (foldTree f x r)) l
  -- let right = foldTree f x r
  --     mid = f a right
  --     left = foldTree f mid l
  -- in left

--     1
--  2     3
-- 4 5
-- inorder
-- [4, 2, 5, 1, 3]

-- (:) :: a -> [a] -> [a]
-- (:) x xs = x:xs
--
-- 1
--2 .
-- inorder (Node (Node Leaf 2 Leaf) 1 Leaf)
-- = foldTree (:) [] (Node (Node Leaf 2 Leaf) 1 Leaf)
-- = foldTree (:) [] (Node Leaf 2 Leaf) : 1
-- = 2 : 1 : []

inorder :: Tree a -> [a]
inorder = foldTree (:) []

instance Foldable Tree where
  foldr = foldTree

-- data List = [] | x : []
-- instance Foldable List where
--   foldr f y (x:xs) = f (foldr f y xs) x

{-T7.3-}

-- rules:
-- (\x -> x + 1) => (+1) 
-- f x = g x => f = g
-- h x = f $ g x => h = f . g
-- f x = g (h (i x)) => f = g . h . i
-- map f . map g => map (f . g)
-- (\f acc -> f acc) => ($)
-- f x y = x => const

-- Control.Applicative <*>
-- h x = f x (g x) => h = f <*> g

f1 = map (+1)
f2 = map ((*2) . (+1))
f3 = filter (> 1) . map (+ 1)
f4 = foldr ($) 0 . map ($5)
f5 = (.)
f6 = uncurry
-- f7 f x y z = f z y
-- f7 f x y z = flip (f x) y z
-- f7 f x = flip (f x)
-- f7 f x = const . flip
f7 = const . flip

{-
-- Note to tutors: at this point, please explain partial applications again
f1' = map (+1)

f2' = map (2*) . map (+1)
f2'' = map ((2*) . (+1))

f3' = filter (>1) . map (+1)

f4' fs = foldr (\f acc -> f acc) 0 (map ($5) fs)
f4'' = foldr (\f acc -> f acc) 0 . map ($5)
-- Type inference fails after this
f4''' :: (Num a,Num b) => [(a->b->b)] -> b
f4''' = foldr (\f acc -> (f 5) acc) 0 
f4'''' :: (Num a,Num b) => [(a->b->b)] -> b
f4'''' = foldr (\f -> (f 5)) 0
f4''''' :: (Num a,Num b) => [(a->b->b)] -> b
f4''''' = foldr ($5) 0

f5' f g = f . g
f5'' f = (.) f
f5''' = (.)
-- Note to tutors: at this point, please explain currying again
f6' = uncurry

f7' f x = flip f 
f7'' f = const (flip f)
f7''' = const . flip
-}
f8 f g x y = f (g x y)
f8' f g x = f . g x
f8'' f g x = (f.) (g x)
-- f (g x) = (f . g) x for any functions f, g
f8''' f g = ((f.) . g)
f8'''' f g = (.) (f.) g
f8''''' f = (.) (f.)
f8'''''' f = (.) ((.) f)
f8''''''' f = ((.) . (.)) f
-- It's the Haskell owl!
f8'''''''' = (.).(.)
