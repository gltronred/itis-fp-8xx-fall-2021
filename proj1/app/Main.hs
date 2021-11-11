module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

main = do
  print "enter value for N: "
  input <- getLine
  let value = (read input :: Int)
  threads <- getNumCapabilities
  print threads
  qu <- atomically newTQueue
  withAsync (printRes qu value) $ \res -> do
    summa <- replicateConcurrently threads $
      count qu r 0
    wait r
    print summa
    print $ sum summa

verifyPrime :: Int -> Int
verifyPrime 1 = 0
verifyPrime n
  = if g n 2
    then n
    else 0
  where g n k | k*k > n = True
              | n `mod` k == 0 = False
              | otherwise = g n (k+1)

count :: TQueue Int -> Async () -> Int -> IO Int
count qu r s = do
  (finish, m) <- atomically $ (,)
    <$> pollSTM r
    <*> tryReadTQueue qu
  case m of
    Nothing -> do
      case finish of
        Nothing -> count qu r s
        Just _ -> pure s
    Just x -> count qu r $! s + verifyPrime x

printQ :: TQueue Int -> Int -> IO ()
printQ qu n = do
  print ("Our N: " ++ (show n))
  forM_ [1..n] $ \i ->
    atomically $ writeTQueue qu i