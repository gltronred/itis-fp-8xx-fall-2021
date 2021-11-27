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

-- !!!!!!!!!!!!!!!!!!!!!! ещё одна версия решения через вспомогательную функцию с изготовление строки !!!!!!!!!!!!!!!!!!!!

-- sumAndTabulate :: Int -> [Int] -> (Int, [Char])
-- sumAndTabulate cols list = helper cols list 1 0 ""
--   where 
--     helper cols []     col sum str = if col == 1 then (sum, str) else (sum, str ++ "\n")
--     helper cols (x:xs) col sum str |cols == col = helper cols xs 1       (sum+x) (str++(show x)++"_:_" ++ "\n")
--                                  | otherwise   = helper cols xs (col+1) (sum+x) (str++(show x)++"_:_" )



-- main :: IO ()
-- main = do
--   let (s, result) = sumAndTabulate 4 [1..20]
--   putStrLn (show s)
--   putStr result

-- another version with onprocess-print

sumAndTabulate cols list delimiter = do
  sum <- printAndSum cols cols list delimiter
  putStr "\n"
  putStr $ show sum
 
printAndSum _ _ [] _ = return 0
printAndSum currentColumn countColumns (el:rest) delimiter = do
  if currentColumn == countColumns then putStr "\n"
    else putStr ""
  putStr $ delimiter ++ show el
  if (currentColumn /= 1) then do
    sum <- printAndSum (currentColumn - 1) countColumns rest delimiter
    return $ sum + el
  else do
    sum <- printAndSum countColumns countColumns rest delimiter
    return $ sum + el

main :: IO ()
main = sumAndTabulate 5 [1..25] "_:_"
