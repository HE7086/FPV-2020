module Tut07 where

import Data.Ratio
import Data.Function
import GHC.Num

{-T7.1-}
data Fraction = Over Integer Integer
-- Fraction -> Ratio
-- Over -> Data.Ratio.(%) 

norm :: Fraction -> Fraction
norm (Over a b) = (a `div` c) `Over` (b `div` c)
  where c = gcd a b

instance Num Fraction where
  (a1 `Over` b1) + (a2 `Over` b2) = norm $ (a1*b2 + a2*b1) `Over` (b1 * b2)
  -- (+) (a1 `Over` b1) (a2 `Over` b2) = norm $ (a1*b2 + a2*b1) `Over` (b1 * b2)
  -- infix
  (a1 `Over` b1) - (a2 `Over` b2) = norm $ (a1*b2 - a2*b1) `Over` (b1 * b2)
  (a1 `Over` b1) * (a2 `Over` b2) = norm $ (a1 * a2) `Over` (b1 * b2)
  negate (a `Over` b) = negate a `Over` b
  fromInteger n = n `Over` 1
  abs (a `Over` b) = abs a `Over` abs b
  signum (a `Over` b) = (signum a * signum b) `Over` 1

  -- examples using Data.Function.on
  -- abs (a `Over` b) = (Over `on` abs) a b
  -- signum (a `Over` b) = (timesInteger `on` signum) a b `Over` 1

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
sumTree (Node l v r) = sumTree l + v + sumTree r

cut :: Tree a -> Integer -> Tree a
cut Leaf _ = Leaf
cut (Node l v r) n
  | n <= 0 = Leaf
  | otherwise = Node (cut l (n-1)) v (cut r (n-1))

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree _ i Leaf = i
foldTree f i (Node l a r) = 
  let right = foldTree f i r
      mid = f a right
      left = foldTree f mid l
  in left

inorder :: Tree a -> [a]
inorder = foldTree (:) []

instance Foldable Tree where
  foldr = foldTree

{-T7.3-}
-- Note to tutors: at this point, please explain partial applications again
f1 :: [Integer] -> [Integer]
f1 = map (+1)

-- alternate solution: for minimizing tokens
-- f1 = map succ

f2 :: [Integer] -> [Integer]
--f2 = map (2*) . map (+1)
f2 = map ((2*) . (+1))

-- alternate solution: for minimizing tokens
--f2 = map $ timesInteger 2 . succ

f3 :: [Integer] -> [Integer]
f3 = filter (>1) . map (+1)
-- alternate solution: for minimizing tokens
--f3 = filter (>1) . map succ

f4 :: (Num t, Num a) => [a -> t -> t] -> t
--f4 = foldr ((\f acc -> f acc) . ($5)) 0
--f4 fs = foldr (\f acc -> f acc) 0 (map ($5) fs)
--f4 = foldr (\f acc -> (f 5) acc) 0 
--f4 = foldr (\f -> (f 5)) 0
f4 = foldr ($5) 0

f5 :: (b -> c) -> (a -> b) -> a -> c
-- f5 f g = f . g
-- f5 f = (.) f
f5 = (.)

-- Note to tutors: at this point, please explain currying again
f6 :: (a -> b -> c) -> (a, b) -> c
f6 = uncurry
-- curry :: ((a, b) -> c) -> a -> b -> c

f7 :: (a -> b -> c) -> p -> b -> a -> c
--f7 f x = flip f 
--f7 f = const (flip f)
f7 = const . flip

f8 :: (b -> c) -> (t -> a -> b) -> t -> a -> c
--f8 f g x = (f.) (g x)
--f8 f g x = f . g x
--f (g x) = (f . g) x for any functions f, g
-- f8 f g = ((f.) . g)
-- f8 f g = (.) (f.) g
-- f8 f = (.) (f.)
-- f8 f = (.) ((.) f)
-- f8 f = ((.) . (.)) f
-- It's the Haskell owl!
f8 = (.) . (.)



{-
   *    *  ()   *   *   
*        * /\         * 
      *   /i\\    *  *  
    *     o/\\  *      *
 *       ///\i\    *    
     *   /*/o\\  *    * 
   *    /i//\*\      *  
        /o/*\\i\   *    
  *    //i//o\\\\     * 
    * /*////\\\\i\*     
 *    //o//i\\*\\\   *  
   * /i///*/\\\\\o\   * 
  *    *   ||     *     
Merry Christmas and Happy New Year
-}
