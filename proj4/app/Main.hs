module Main where

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
  helper cols list 1
  putStrLn "\n"
  return (sum list)

helper cols (x : xs) current
  | cols == current = printLast cols (x : xs)
  | null xs = printElement (x : xs)
  | otherwise = printAndGoNext cols (x : xs) current

printLast cols (x : xs) = do
    putStrLn ("\t" ++ show x)
    helper cols xs 1

printElement (x : xs) = putStr ("\t" ++ show x)

printAndGoNext cols (x : xs) current = do
    putStr ("\t" ++ show x)
    helper cols xs (current + 1)

main :: IO Int
main = sumAndTabulate 4 [1 .. 15]
