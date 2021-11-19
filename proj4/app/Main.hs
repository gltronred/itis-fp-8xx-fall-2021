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
sumAndTabulate :: Int -> [Int] -> IO ()
sumAndTabulate cols list = do
       let (a,b) = iter cols list 1 0 ""
       putStr b
       print a

iter cols []     col sum str = if col == 1 then (sum, str) else (sum, str ++ "\n")
iter cols (x:xs) col sum str | cols == col = iter cols xs 1       (sum+x) (str++(show x)++"\t\n")
                             | otherwise   = iter cols xs (col+1) (sum+x) (str++(show x)++"\t" )

main:: IO ()
main = sumAndTabulate 3 [1..10]
