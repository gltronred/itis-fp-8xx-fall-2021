module Main where

import Streaming as S
import qualified Streaming.Prelude as S

-- import Control.Lens

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
  let (sum, lines) = helper cols list 1 0 ""
  putStrLn lines
  print sum

helper cols [] curCol sum lines = (sum, lines)
helper cols (x : xs) curCol sum lines
  | curCol `mod` cols == 0 = helper cols xs (curCol + 1) (sum + x) (lines ++ show x ++ "\n") 
  | otherwise = helper cols xs (curCol + 1) (sum + x) (lines ++ show x ++ "\t")

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

type Lens f s t a b
  = (a -> f b) -- функция модификации поля
  -> s         -- старый объект
  -> f t       -- новый объект
type Lens' f s a = Lens f s s a a

ix :: Functor f => Int -> Lens' f [a] a
ix k = go k
  where
    go 0 f (x:xs) = (:xs) <$> f x
    go k f (x:xs) = (x:) <$> go (k-1) f xs

_1 :: Functor f => Lens f (a,b) (c,b) a c
_1 f (a,b) = (\x -> (x,b)) <$> f a

_2 :: Functor f => Lens f (a,b) (a,d) b d
_2 f (a,b) = (\x -> (a,x)) <$> f b

ex1 = (ix 4) Identity [1..10]
ex2 = (ix 4) (const $ Identity 101) [1..10]
ex3 = (ix 4) (\x -> [101..104]) [1..10]
ex4 = (ix 4) (\x -> (x,x)) [1..10]
ex5 = _1 (const $ Identity 789) (123,456)
ex6 = _1 (const $ Identity "ads") (123,456)
ex7 = _2 (Identity . show) (123,456)
ex8 = _2 (\x -> [show x, show $ x+1]) (123,456)
ex9 = (_1 . _2) (const $ Identity "x") ((1,2),3)




{-------------------
*Main> ((1,[3::Int,4,5]),(2,3))^._2._2
3
*Main> ((1,[3::Int,4,5]),(2,3)) & _2._2 .~ "asd"
((1,[3,4,5]),(2,"asd"))
*Main> ((1,[3::Int,4,5]),(2,3)) & _2._2 .~ (123,321)
((1,[3,4,5]),(2,(123,321)))
*Main> ((1,[3::Int,4,5]),(2,3)) ^. _2
(2,3)
*Main> ((1,[3::Int,4,5]),(2,3)) & _2 %~ (\(a,b) -> (a,b,123))
((1,[3,4,5]),(2,3,123))
*Main> ((1,[3::Int,4,5]),(2,3)) ^. _1 . _2
[3,4,5]
*Main> ((1,[3::Int,4,5]),(2,3)) ^? _1 . _2 . ix 5
Nothing
*Main> ((1,[3::Int,4,5]),(2,3)) ^? _1 . _2 . ix 1
Just 4
*Main> ((1,[3::Int,4,5]),(2,3)) ^.. _1 . _2 . ix 1
[4]
*Main> Right 123 :: Either String Int
Right 123
*Main> let ea = Right 123 :: Either String Int
*Main> ea ^? _Left
Nothing
*Main> ea ^? _Right
Just 123
*Main> ea & _Right .~ 124
Right 124
*Main> ea & _Left .~ 124
Right 123
*Main> ea & _Left .~ "123"
Right 123
*Main> ea & _Left %~ ("f" ++)
Right 123
*Main> let eb = Left "asdasd"
*Main> eb & _Left %~ ("f" ++)
Left "fasdasd"
*Main> ea & _Left %~ ("f" ++)
Right 123
*Main> ea & _Left %~ ("f" ++)
Right 123
------------------}

main :: IO ()
main = sumAndTabulate 4 [1 .. 15]
