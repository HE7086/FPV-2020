module Tut09 where

{-T9.1-}
data NonEmptyList a = Single a | a `Cons` (NonEmptyList a)
  deriving (Show, Eq)

toList :: NonEmptyList a -> [a]
toList (Single x) = [x]
toList (x `Cons` xs) = x : toList xs

fromList :: [a] -> Maybe (NonEmptyList a)
fromList [] = Nothing
fromList (x : xs) = Just (helper x xs)
  where helper a [] = Single a
        helper a (y:ys) = a `Cons` helper y ys

-- fromList1 :: [a] -> Maybe (NonEmptyList a)
-- fromList1 [] = Nothing
-- fromList1 [x] = Just $ Single x
-- fromList1 (x : xs) = Cons x <$> fromList1 xs
-- fromList1 (x : xs) = Just(Cons x) <*> fromList1 xs

nHead :: NonEmptyList a -> a
nHead (Single x) = x
nHead (x `Cons` _) = x

nTail :: NonEmptyList a -> Maybe (NonEmptyList a)
nTail (Single _) = Nothing
nTail (_ `Cons` t) = Just t

nAppend :: NonEmptyList a -> NonEmptyList a -> NonEmptyList a
(Single x) `nAppend` ys = x `Cons` ys
(x `Cons` xs) `nAppend` ys = x `Cons` (xs `nAppend` ys)

nTake :: Integer -> NonEmptyList a -> Maybe (NonEmptyList a)
nTake n _ | n <= 0 = Nothing
nTake n a@(Single x)
  | n == 1 = Just a
  | otherwise = Nothing
nTake n (x `Cons` xs)
  | n == 1 = Just $ Single x
  | otherwise = case nTake (n - 1) xs of
                  Nothing -> Nothing
                  Just ys -> Just $ x `Cons` ys

  -- | otherwise = if isJust a@(nTake (n - 1) xs)
  --                  then Just $ (x `Cons` fromJust a)
  --                  else Nothing
  --
  -- | otherwise = Cons x <$> nTake (n - 1) xs
--
-- f <$> Nothing -> Nothing
-- f <$> Just a  -> Just (f a)

-- take 3 [1,2,3,4]
-- 1 : take 2 [2,3,4]
-- 1:2: take 1 [3,4]
-- 1:2:3:[]
--

{-T9.3-}
type Name = String
data Polarity = Pos | Neg
  deriving Eq
data Literal = Literal Polarity Name
  deriving Eq
type Clause = [Literal]
type ConjFrom = [Clause]

--(a || b) && (c || d)
-- instance Show Polarity where
--   show Pos = ""
--   show Neg = "~"

instance Show Literal where
  show (Literal Pos a) = show a
  show (Literal Neg a) = "~" ++ show a

lName :: Literal -> Name
lName (Literal _ n) = n

lPos :: Name -> Literal
lPos = Literal Pos

lNeg :: Name -> Literal
lNeg = Literal Neg

lIsPos :: Literal -> Bool
lIsPos (Literal p _) = case p of
                         Pos -> True
                         Neg -> False
-- lIsPos (Literal Pos _) = True
-- lIsPos (Literal Neg _) = False

lIsNeg :: Literal -> Bool
lIsNeg = not . lIsPos

lNegate :: Literal -> Literal
lNegate (Literal Pos n) = Literal Neg n
lNegate (Literal Neg n) = Literal Pos n

complements :: Literal -> Literal -> Bool
complements (Literal p n) (Literal p' n') = n == n' && p /= p'

type Valuation = [Name]

evalLiteral :: Valuation -> Literal -> Bool
evalLiteral val l@(Literal _ n)
  | lIsPos l  = n `elem` val
  | otherwise = n `notElem` val

evalClause :: Valuation -> Clause -> Bool
evalClause = any . evalLiteral

eval :: Valuation -> ConjForm -> Bool
eval = all . evalClause

clauseIsTauto :: Clause -> Bool
clauseIsTauto [] = False
clauseIsTauto (l:ls) = lNegate l `elem` ls || clauseIsTauto ls
