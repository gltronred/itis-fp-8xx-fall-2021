module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

isPrime :: Int -> Int -> Int
isPrime 1 _ = 0
isPrime n m
  = if m * m > n
    then n
    else if (n `mod` k == 0)
      then 0
      else primeOrZero n (m + 1)

count :: TQueue Int -> Async () -> Int -> IO Int
count q r s = do
  (finished, mx) <- atomically $ (,)
    <$> pollSTM r
    <*> tryReadTQueue q
  case mx of
    Nothing -> do
      -- finished <- poll r
      case finished of
        Nothing -> count q r s
        Just _ -> pure s
    Just x -> count q r $! s + isPrime x 2

writer :: TQueue Int -> Int -> IO ()
writer q n = do
  forM_ [1..n] $ \i ->
    atomically $ writeTQueue q i

main = do
  threads <- getNumCapabilities
  putStrLn "Print n:"
  str <- getLine
  let n = (read str :: Int)
  q <- atomically newTQueue
  withAsync (writer q n) $ \r -> do
    sums <- replicateConcurrently threads $
      count q r 0
    wait r
    print sums
    print $ sum sums