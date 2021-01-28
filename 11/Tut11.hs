module Tut11 where

import Data.Maybe (fromJust)
import Data.List (nub, sort, delete)
import qualified AssocList as AL
import qualified Data.Map as DM

import Test.QuickCheck
import Data.Function

{-T12.1.2-}
prop_invarEmpty :: Bool
prop_invarEmpty = AL.invar (AL.empty :: AL.Map Int Int) -- need to explicitly state type variables for this test

prop_invarInsert :: Eq k => k -> v -> AL.Map k v -> Property
prop_invarInsert k v m = AL.invar m ==> AL.invar (AL.insert k v m)

prop_invarDelete :: Eq k => k -> AL.Map k v -> Property
prop_invarDelete k m = AL.invar m ==> AL.invar (AL.delete k m)

hom :: Ord k => AL.Map k v -> DM.Map k v
hom m = foldr (\k -> DM.insert k $ fromJust $ AL.lookup k m) DM.empty $ AL.keys m

prop_simEmpty :: Bool
prop_simEmpty = hom (AL.empty :: AL.Map Integer String) == DM.empty

prop_simInsert :: (Ord k, Eq v) => k -> v -> AL.Map k v -> Property
prop_simInsert k v m = AL.invar m ==> hom (AL.insert k v m) == DM.insert k v (hom m)

prop_simLookup :: (Ord k, Eq v) => k -> AL.Map k v -> Property
prop_simLookup k m = AL.invar m ==> AL.lookup k m == DM.lookup k (hom m)

prop_simDelete :: (Ord k, Eq v) => k -> AL.Map k v -> Property
prop_simDelete k m = AL.invar m ==> hom (AL.delete k m) == DM.delete k (hom m)

prop_simKeys :: Ord k => AL.Map k v -> Property
prop_simKeys m = AL.invar m ==> sort (AL.keys m) == DM.keys (hom m)

data Term = App Term Term | Abs String Term | Var String

instance Show Term where
  show (Var x) = x
  show (Abs x t) = "(\\" ++ x ++ " -> " ++ show t ++ ")"
  show (App t1 t2@(App _ _)) = show t1 ++ " (" ++ show t2 ++ ")"
  show (App t1 t2) = show t1 ++ " " ++ show t2

freeVars :: Term -> [String]
freeVars = freeVarsB []
  where
    freeVarsB bs (Var x)
      | x `elem` bs = []
      | otherwise = [x]
    freeVarsB bs (App t1 t2) = nub $ freeVarsB bs t1 ++ freeVarsB bs t2
    freeVarsB bs (Abs x t) = freeVarsB (x:bs) t

substVar :: String -> Term -> Term -> Term
substVar v term (Var x)
  | v == x = term 
  | otherwise = Var x
substVar v term (Abs x t)
  | v == x = Abs x t
  | otherwise = Abs x (substVar v term t)
substVar v term (App t1 t2) = App (substVar v term t1) (substVar v term t2)

-- Capture-avoiding substitution
varNames :: [String]
varNames = tail vns
  where
    vns = "" : [c:v | v <- vns, c <- ['a'..'z']]

freshVarnames :: Term -> [String]
freshVarnames t = [v | v <- varNames, v `notElem` freeVars t]

renameBounds :: Term -> Term
renameBounds t = renameBounds' (freshVarnames t) t
  where
    renameBounds' _ (Var x) = Var x
    renameBounds' vs (App t1 t2) = on App (renameBounds' vs) t1 t2
    renameBounds' vs (Abs x _) =
      let
        tr = renameBounds' (delete x $ tail vs) t
        ts = substVar x (Var $ head vs) tr
       in Abs (head vs) ts 

substVar' :: String -> Term -> Term -> Term 
substVar' v r t = case renameBounds (App (Abs v t) r) of
                    App (Abs v' t') r' -> substVar v' r' t'
                    _ -> undefined
