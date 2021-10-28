module Main where

import Control.Concurrent
import Control.Concurrent.Async

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

main :: IO ()
main = do
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
