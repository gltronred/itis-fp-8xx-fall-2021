module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

getNumber :: Int -> Int
getNumber 1 = 0
getNumber n = if (isPrime n 2) then n else 0

isPrime :: Int -> Int-> Bool
isPrime n k      
            | k*k > n = True
            | n `mod` k == 0 = False
            | otherwise = isPrime n (k+1)

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
    Just x -> count q r $! s + getNumber x

writer :: TQueue Int -> Int -> IO ()
writer q n = do
  forM_ [1..n] $ \i ->
    atomically $ writeTQueue q i

main :: IO ()
main = do
  threads <- getNumCapabilities

  putStrLn "value for n:"
  input <- getLine
  let n = (read input :: Int)

  q <- atomically newTQueue
  withAsync (writer q n) $ \r -> do
    sums <- replicateConcurrently threads $
      count q r 0
    wait r
    print sums
    print $ sum sums
