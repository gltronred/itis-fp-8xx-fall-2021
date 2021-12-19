module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

data D = D Integer Integer

printPrime :: Integer -> IO ()
printPrime 1 = putStrLn "One"
printPrime n
  = if go n 2
    then putStrLn ("Prime " ++ show n)
    else putStrLn ("Not prime " ++ show n)
  where go n k | k*k > n = True
               | n `mod` k == 0 = False
               | otherwise = go n (k+1)

pp :: Integer -> IO Integer
pp x = printPrime x >> pure x

list :: [Integer]
list = [ 66333293675293
       , 37446796642823
       , 59978924802163
       , 79404304192991
       , 14833513069799
       , 70451647138309
       , 62204847621503
       , 83747881716781
       , 53222479319519
       , 34298041863719
       ]

main1 :: IO ()
main1 = do
  getNumCapabilities >>= print
  let x = list!!0
      y = list!!1
  -- withAsync (printPrime x) $ \rx -> do
  --   withAsync (printPrime y) $ \ry -> do
  --     wait rx
  --     wait ry
  --
  -- forConcurrently_ list printPrime
  D a b <- runConcurrently $ D
    <$> Concurrently (pp x)
    <*> Concurrently (pp y)
  print a
  print b

summator :: TBQueue Int -> TVar Bool -> Int -> IO Int
summator q b s = do
  mx <- atomically $ do
    finished <- readTVar b
    if finished
      then tryReadTBQueue q
      else Just <$> readTBQueue q
  case mx of
    Just x -> summator q b $! s+x
    Nothing -> pure s

fillQueue :: TBQueue Int -> TVar Bool -> IO ()
fillQueue q b = do
  let n = 10000000
  forM_ [1..n] $ \i ->
    atomically $ writeTBQueue q i
  atomically $ writeTVar b True

main2 = do
  threads <- getNumCapabilities
  print threads
  (q,b) <- atomically $ (,)
    <$> newTBQueue 10000000
    <*> newTVar False
  withAsync (fillQueue q b) $ \r -> do
    results <- replicateConcurrently threads $
      summator q b 0
    wait r
    forM_ results print
    print $ sum results

-- Повторяем n раз действие,
-- пока возвращается Nothing
-- Если вернулся Just, возвращаем его
retryN :: Int -> STM (Maybe a) -> STM (Maybe a)
retryN 0 act = pure Nothing
retryN n act = do
  mr <- act
  case mr of
    Nothing -> retryN (n-1) act
    Just r -> pure $ Just r

count :: TQueue Int -> Async () -> Int -> IO Int
count q r s = do
  mx <- atomically $ tryReadTQueue q
  case mx of
    Nothing -> do
      finished <- poll r
      case finished of
        Nothing -> count q r s
        Just _ -> pure s
    Just x -> count q r $! s + (if primeCheck x then x else 0)

primeCheck n = primeCheck' n 2 where
  primeCheck' n i | i^2 > n = True
                  | n `mod` i == 0 = False
                  | otherwise = primeCheck' n (i + 1)

writer :: TQueue Int -> IO ()
writer q = do
  forM_ [1..1000000] $ \i ->
    atomically $ writeTQueue q i

main = do
  threads <- getNumCapabilities
  print threads
  q <- atomically newTQueue
  withAsync (writer q) $ \r -> do
    sums <- replicateConcurrently threads $
      count q r 0
    wait r
    print sums
    print $ sum sums
