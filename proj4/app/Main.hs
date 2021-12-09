module Main where

import Data.List ()

-- Вывести на экран список list
-- в cols столбцов (например,
-- после каждого числа вывести
-- знак табуляции) и вернуть
-- сумму элементов списка
--
-- > sumAndTabulate 3 [1..10]
-- >
-- >    1       2       3
-- >    4       5       6
-- >    7       8       9
-- >    10
-- >
-- > 55
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
main = putStrLn "Hello, Haskell!"
