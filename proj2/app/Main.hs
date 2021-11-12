module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad


getPrimeSum :: Int -> Int
getPrimeSum 1 = 0
getPrimeSum n = if go n 2
    then n
    else 0
  where go m k | k*k > m = True
               | m `mod` k == 0 = False
               | otherwise = go m (k+1)

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
    Just x -> count q r $! s + getPrimeSum x

writer :: TQueue Int -> Int -> IO ()
writer q n = do
  forM_ [1..n] $ \i ->
    atomically $ writeTQueue q i

getInt :: IO Int
getInt = read <$> getLine

main :: IO ()
main = do
  putStrLn "Type threads count:"
  threadsInp <- getInt
  setNumCapabilities threadsInp
  threads <- getNumCapabilities
  putStrLn $ "Running with " ++ show threads ++ " threads"

  putStrLn "Type N:"
  n <- getInt

  q <- atomically newTQueue
  withAsync (writer q n) $ \r -> do
    sums <- replicateConcurrently threads $
      count q r 0
    wait r
    putStrLn "Sums:"
    print sums
    putStrLn "Final sum:"
    print $ sum sums
