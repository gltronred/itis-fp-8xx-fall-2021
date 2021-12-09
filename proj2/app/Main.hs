module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Numbers.Primes

data D = D Integer Integer

printPrime :: Integer -> IO ()
printPrime 1 = putStrLn "One"
printPrime n =
  if go n 2
    then putStrLn ("Prime " ++ show n)
    else putStrLn ("Not prime " ++ show n)
  where
    go n k
      | k * k > n = True
      | n `mod` k == 0 = False
      | otherwise = go n (k + 1)

pp :: Integer -> IO Integer
pp x = printPrime x >> pure x

list :: [Integer]
list =
  [ 66333293675293,
    37446796642823,
    59978924802163,
    79404304192991,
    14833513069799,
    70451647138309,
    62204847621503,
    83747881716781,
    53222479319519,
    34298041863719
  ]

main1 :: IO ()
main1 = do
  getNumCapabilities >>= print
  let x = head list
      y = list !! 1
  -- withAsync (printPrime x) $ \rx -> do
  --   withAsync (printPrime y) $ \ry -> do
  --     wait rx
  --     wait ry
  --
  -- forConcurrently_ list printPrime
  D a b <-
    runConcurrently $
      D
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
    Just x -> summator q b $! s + x
    Nothing -> pure s

fillQueue :: TBQueue Int -> TVar Bool -> IO ()
fillQueue q b = do
  let n = 10000000
  forM_ [1 .. n] $ \i ->
    atomically $ writeTBQueue q i
  atomically $ writeTVar b True

main2 :: IO ()
main2 = do
  threads <- getNumCapabilities
  print threads
  (q, b) <-
    atomically $
      (,)
        <$> newTBQueue 10000000
        <*> newTVar False
  withAsync (fillQueue q b) $ \r -> do
    results <-
      replicateConcurrently threads $
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
    Nothing -> retryN (n -1) act
    Just r -> pure $ Just r

count :: TQueue Int -> Async () -> Int -> IO Int
count q r s = do
  valueFromQueue <- atomically $ tryReadTQueue q
  case valueFromQueue of
    Nothing -> do
      finished <- poll r
      case finished of
        Nothing -> count q r s
        Just _ -> pure s
    Just x -> count q r $! s + processPrimeness x

processPrimeness :: Int -> Int
processPrimeness x =
  if isPrime x
    then x
    else 0

writer :: TQueue Int -> IO ()
writer q = writerN q 1000000

writerN :: TQueue Int -> Int -> IO ()
writerN q n = do
  forM_ [1 .. n] $ \i ->
    atomically $ writeTQueue q i

sumPrimeNumbers :: Int -> IO ()
sumPrimeNumbers n = do
  q <- atomically newTQueue
  threads <- getNumCapabilities
  withAsync (writerN q n) $ \r -> do
    sums <- replicateConcurrently threads (count q r 0)
    print sums
    print $ sum sums

main :: IO ()
main = sumPrimeNumbers 1000000
