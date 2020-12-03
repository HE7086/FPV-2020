module Tut04 where

{-T4.1-}
lSub :: Num a => [a] -> [a]
lSub [] = []
lSub [x] = [x]
lSub (x:y:xs) = (x-y):lSub (y:xs)

noDupSnoc :: Eq a => [a] -> a -> [a]
noDupSnoc [] y = [y]
noDupSnoc (x:xs) y
  | x == y = x:xs
  | otherwise = x:noDupSnoc xs y

addAbsLt :: (Num a, Ord a) => [a] -> a -> a
-- addAbsLt :: Num a => Ord a => [a] -> a -> a
addAbsLt [] _ = 0
addAbsLt (x:xs) y
  | x < y = abs x + addAbsLt xs y
  | otherwise = addAbsLt xs y 
  
-- addAbsLt = sum . map abs

{-T4.3-}
addAbsLt2 :: (Num a, Ord a) => [a] -> a -> a
addAbsLt2 xs y = aux xs 0
  where
    aux [] a = a
    aux (x:xs) a
      | x < y = aux xs (abs x + a)
      | otherwise = aux xs a

maxAbs :: (Ord a, Num a) => [a] -> a
maxAbs xs = aux xs 0
  where
    aux [] a = a
    aux (x:xs) a = aux xs $ max (abs x) a
    
-- maxAbs [] = 0
-- maxAbs xs = maximum $ map abs xs

countSigns :: (Eq a, Num a) => [a] -> (a,a,a)
countSigns xs = aux xs 0 0 0
  where
    aux [] n z p = (n,z,p)
    aux (x:xs) n z p
      | signum x == -1 = aux xs (n+1) z p
      | signum x == 0 = aux xs n (z+1) p
      | otherwise = aux xs n z (p+1)
--
-- countSigns xs = (length (filter (<0) xs), length (filter (==0) xs), length (filter >0) xs)
--
-- fancier way, but you need to convert it to tuple
-- countSigns xs = map length $ groupBy signum $ sort xs

ltAndGt :: Ord a => [a] -> a -> Bool
ltAndGt xs y = aux xs False False
  where
    aux [] foundLt foundGt = foundLt && foundGt
    aux (x:xs) foundLt foundGt = aux xs (foundLt || x < y) (foundGt || x > y)

-- ltAndGt xs y = 2 == length $ groupBy (>y) $ filter (/= y) $ sort xs

{-T4.4-}
ups :: Ord a => [a] -> [[a]]
ups [] = []
ups [x] = [[x]]
ups (x:y:zs)
  | x <= y =  (x:us) : uss
  | otherwise =  [x] : us : uss
  where
    (us:uss) = ups (y:zs) 
