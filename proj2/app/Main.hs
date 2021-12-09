module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

count :: TQueue Int -> Async () -> Int -> IO Int
count q r s = do
  (finished, mx) <- atomically $ (,)
    <$> pollSTM r
    <*> tryReadTQueue q
  case mx of
    Nothing -> do
      case finished of
        Nothing -> count q r s
        Just _ -> pure s
    Just x -> count q r $! s + if isPrime x then x else 0

writer :: TQueue Int -> Int -> IO ()
writer q n = do
  forM_ [1..n] $ \i ->
    atomically $ writeTQueue q i

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = go n 2
  where go n k | k*k > n =        True
               | n `mod` k == 0 = False
               | otherwise = go n (k+1)

main :: IO ()
main = do
  threads <- getNumCapabilities

  let n = 100000

  q <- atomically newTQueue
  withAsync (writer q n) $ \r -> do
    sums <- replicateConcurrently threads $
      count q r 0
    wait r
    print sums
    print $ sum sums
