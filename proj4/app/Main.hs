module Main where

import Streaming as S
import qualified Streaming.Prelude as S

import TheLens ()

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
sumAndTabulate cols list = inner 0 cols cols list where
  inner :: Int -> Int -> Int -> [Int] -> IO Int
  inner sum cols curCol [] = do
    putStrLn ""
    return sum
  inner sum cols 0 list = do
    putStrLn ""
    inner sum cols cols list
  inner sum cols curCol (x:list) = do
    putStr $ show x ++ "\t"
    inner (sum + x) cols (curCol - 1) list

main :: IO ()
main = do
  sum <- sumAndTabulate 5 [1..26]
  putStrLn $ "Сумма = " ++ show sum