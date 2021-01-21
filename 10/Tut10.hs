module Tut10 where

import System.Random (randomRIO)
import Text.Read (readMaybe)

{-T10.1-}
ioReadPrintInt :: IO ()
ioReadPrintInt = do
    x <- getLine
    case readMaybe x :: Maybe Int of
      Nothing -> putStrLn "that is not a number!"
      Just i  -> print i

-- ioReadPrintInt' :: IO Int
-- ioReadPrintInt' = do
--     x <- getLine
--     case readMaybe x of
--       Nothing -> error "that is not a number!"
--       Just i  -> return i

ioLoop :: (a -> Maybe b) -> IO a -> IO b
ioLoop f act = do
  a <- act
  case f a of
    Nothing -> do
      putStrLn "parser error"
      ioLoop f act
    Just b  -> return b

getInt :: IO Int
getInt = ioLoop readMaybe getLine

{-T10.2-}
guessNum :: IO Int
guessNum = do
  number <- randomRIO (0,100)
  putStrLn "Guess a number between 0 and 100"
  doGuess number 1
    where 
      doGuess n count = do
        num <- getInt
        if num < n then do
          putStrLn "too small!"
          doGuess n (count + 1)
        else if num > n then do
          putStrLn "too big!"
          doGuess n (count + 1)
        else do
          putStrLn "you found it!"
          return count

{- T10.3 -}
ioSequence :: [IO a] -> IO [a]
ioSequence [] = return []
ioSequence (ioX:ioXS) = do
  x <- ioX
  xs <- ioSequence ioXS
  return $ x : xs

-- [IO 1, IO 2, IO 3]
-- return $ 1 : ioSeq [IO 2, IO 3]
-- return $ 1 : 2 : ioSeq [IO 3]
-- return $ [1,2,3]

-- ioSequence = sequence

ioSumInts :: IO Int 
ioSumInts = do
  n <- getInt
  xs <- ioSequence $ replicate n getInt
  print xs
  return $ sum xs
