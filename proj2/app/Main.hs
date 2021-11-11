module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import System.Environment


primes n = prime 2 [] n [0..n+1] where
  prime i list n a | i > n       = reverse list
                   | a !! i /= 0 = prime (i+1) ((a !! i) : list) n (chg a i 0) 
                   | otherwise   = prime (i+1) list n a


chg [] _ _ = []
chg (x:list) i n | n `mod` i == 0 = 0:chg list i (n+1)
                 | otherwise      = x:chg list i (n+1)


isPrime x = if primes x /= [] && last (primes x) == x then x else 0



count :: TQueue Int -> Async () -> Int -> IO Int
count q r s = do
  (finished, mx) <- atomically $ (,)
    <$> pollSTM r
    <*> tryReadTQueue q
  case mx of
    Nothing -> do
      finished <- poll r
      case finished of
        Nothing -> count q r s
        Just _ -> pure s
    Just x -> count q r $! s+isPrime x

writer :: TQueue Int -> Int -> IO ()
writer q n = do
  forM_ [1..n] $ \i ->
    atomically $ writeTQueue q i


main = do
  args <- getArgs
  threads <- getNumCapabilities
  print threads
  q <- atomically newTQueue
  withAsync (writer q $ read $ head args) $ \r -> do
    sums <- replicateConcurrently threads $
      count q r 0
    wait r
    print sums
    print $ sum sums


