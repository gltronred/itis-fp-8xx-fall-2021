module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

checkPrime :: Int -> Int
checkPrime 1 = 0
checkPrime n
  = if go n 2
    then n
    else 0
  where go n k | k*k > n = True
               | n `mod` k == 0 = False
               | otherwise = go n (k+1)

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
    Just x -> count q r $! s + checkPrime x

writer :: TQueue Int -> Int -> IO ()
writer q n = do
  print ("Our N: " ++ (show n))
  forM_ [1..n] $ \i ->
    atomically $ writeTQueue q i

main = do
  print "enter value for N: "
  input1 <- getLine
  let x = (read input1 :: Int)
  threads <- getNumCapabilities
  print threads
  q <- atomically newTQueue
  withAsync (writer q x) $ \r -> do
    sums <- replicateConcurrently threads $
      count q r 0
    wait r
    print sums
    print $ sum sums
