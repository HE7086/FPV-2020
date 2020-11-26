module Example where

badSum :: [Integer] -> Integer
badSum xs 
    | null xs = 0
    | otherwise = head xs + badSum (tail xs)

goodSum :: [Integer] -> Integer
goodSum [] = 0
goodSum (x:xs) = x + goodSum xs


--goodSum xs = foldr (+) 0 xs
--goodSum xs = sum xs
--
--
--in ghci:      :set -Wincomplete-type
--then use      :l Example
--to reload and see warnings(remove the base case)
