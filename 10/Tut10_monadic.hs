module Tut10 where

import System.Random (randomRIO)
import Text.Read (readMaybe)
import Control.Monad

{-T10.1-}
ioReadPrintInt :: IO ()
ioReadPrintInt = maybe (putStrLn "not a number") print 
  . (readMaybe :: String -> Maybe Int) 
  =<< getLine

ioLoop :: (a -> Maybe b) -> IO a -> IO b
ioLoop f act = maybe (ioLoop f act) return . f =<< act

getInt :: IO Int
getInt = ioLoop readMaybe getLine

{-T10.2-}
guessNum :: IO Int
guessNum = do
    rnd <- randomRIO (0,100)
    putStrLn "Guess a number between 0 and 100"
    doGuessNum rnd 1
  where
    doGuessNum rnd cnt = do
      num <- getInt
      if num < rnd then do
        putStrLn "The number you are looking for is greater"
        doGuessNum rnd (cnt+1)
      else if num > rnd then do
        putStrLn "The number you are looking for is smaller"
        doGuessNum rnd (cnt+1)
      else do
        putStrLn "You found it!"
        return cnt

{- T10.3 -}
ioSequence :: [IO a] -> IO [a]
ioSequence = foldr (liftM2 (:)) mempty
-- ioSequence = sequence

ioSumInts :: IO Int 
ioSumInts = fmap sum $ sequence . flip replicate getInt =<< getInt

test :: [Int]
test = do
  x <- [1 .. 10]
  y <- [1 .. 10]
  return $ x + y

-- [1+1, 1+2, 1+3 ... 10 + 10]
