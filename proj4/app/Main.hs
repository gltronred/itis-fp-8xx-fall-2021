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
sumAndTabulate cols list = sumTable cols cols list where
  sumTable _ _ [] = do
    putStr "\n"
    return 0
  sumTable current cols (num:list) = do
    if current == cols then putStr "\n"
    else putStr ""
    putStr $ "\t" ++ show num
    if current == 1 then do
      sum <- sumTable cols cols list
      return $ num + sum
    else do
      sum <- sumTable (current - 1) cols list
      return $ num + sum

main :: IO ()
main = do
  sum <- sumAndTabulate 5 [1..23]
  putStrLn $ show sum
