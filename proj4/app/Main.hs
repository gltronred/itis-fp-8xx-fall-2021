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
main = putStrLn "Hello, Haskell!"

sumAndTabulate':: Int -> [Int] -> IO Int
sumAndTabulate' = sumAndTabulate'' 1  

sumAndTabulate'' _ _  [] = do
  putStr "\n"
  return 0

sumAndTabulate'' index itemsPerRow (x:xs) = do
  if index `mod` itemsPerRow == 0
    then do
      putStr $ (show x) ++ "\n"
    else do 
      putStr $ (show x) ++ "\t"
  restSum <- sumAndTabulate'' (index + 1) itemsPerRow xs   
  return $ x + restSum     