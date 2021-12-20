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
sumAndTabulate:: Int -> [Int] -> IO Int
sumAndTabulate 0 _= error "Кол-во столбцов должно быть больше 0"
sumAndTabulate x arr = sumAndTabulateSupport x arr x

sumAndTabulateSupport _ [] _ = do
  putStr "\n\n"
  return 0

sumAndTabulateSupport x (a:arr) i = do
    if (i == 1) then do
        putStr $ (show a) ++ "\n"
        res <- sumAndTabulateSupport x arr x
        return $ a + res
    else do
        putStr $ (show a) ++ "\t"
        res <- sumAndTabulateSupport x arr (i -1)
        return $ a + res

main :: IO ()
main = do
  sum <- sumAndTabulate 3 [1..10]
  putStrLn $ "Сумма: " ++ show sum

