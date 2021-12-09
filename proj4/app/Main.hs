module Main where

import Data.List ()

sumAndTabulate :: Int -> [Int] -> IO Int
sumAndTabulate cols list = do
  s <- prtAndSum 0 cols list
  putStrLn ""
  return s
    where
      prtAndSum :: Int -> Int -> [Int] -> IO Int
      prtAndSum cur _ [] = return cur
      prtAndSum cur cols list = do
        let row = take cols list
        putStrLn $ concatMap (\x -> show x ++ "\t") row
        prtAndSum (cur + sum row) cols (drop cols list)


main :: IO ()
main = do
  sum <- sumAndTabulate 5 [1..23]
  print sum
