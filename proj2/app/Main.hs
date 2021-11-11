module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

data D = D Integer Integer

prime :: Int -> Int
prime 1 = 0
prime n = if go n 2
    then n
    else 0
  where go n k | k*k > n = True
               | mod n k == 0 = False
               | otherwise = go n (k+1)

summator :: TBQueue Int -> TVar Bool -> Int -> IO Int
summator q b s = do
  mx <- atomically $ do
    finished <- readTVar b
    if finished
      then tryReadTBQueue q
      else Just <$> readTBQueue q
  case mx of
    Just x -> summator q b $! s + prime x
    Nothing -> pure s

fillQueue :: TBQueue Int -> TVar Bool -> Int -> IO ()
fillQueue q b x= do
  forM_ [1..x] $ \i ->
    atomically $ writeTBQueue q i
  atomically $ writeTVar b True

main = do
  y <- getLine
  let x = (read y :: Int)
  threads <- getNumCapabilities
  print threads
  (q,b) <- atomically $ (,)
    <$> newTBQueue 10000000
    <*> newTVar False
  withAsync (fillQueue q b x) $ \r -> do
    results <- replicateConcurrently threads $
      summator q b 0
    wait r
    forM_ results print
    print $ sum results
