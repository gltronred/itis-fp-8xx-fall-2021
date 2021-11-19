module Main where

import Streaming as S
import qualified Streaming.Prelude as S

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
sumAndTabulate cols list = error "Write me!"


-- -- Свободная монада - на одном из следующих занятий
-- data S f m r
--   = Elem (f (S f m r)) -- "чистый" элемент e и данные дальше
--   | Act (m (S f m r))  -- данные из действия (в монаде m)
--   | Res r              -- результат r

stS :: Int
    -> Stream (Of Int) IO Int
    -> IO Int
stS cols = fmap S.fst' .
  S.sum .
  S.mapM_ (liftIO . putStrLn) .
  tabS 3 .
  S.copy

-- Получение стрима из нужных строк
tabS :: Monad m
     => Int
     -> Stream (Of Int) m r
     -> Stream (Of String) m r
tabS cols ints = S.mapsM S.mconcat $
                 S.chunksOf cols $
                 S.map addTab ints
  where addTab x = show x ++ "\t"

-- Вывод на экран
outTabS :: Int
        -> Stream (Of Int) IO ()
        -> IO ()
outTabS cols = S.mapM_ putStrLn .
               tabS cols

--------------------------------------

main :: IO ()
main = putStrLn "Hello, Haskell!"
