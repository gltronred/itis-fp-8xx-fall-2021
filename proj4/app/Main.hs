{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Lens.TH
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Streaming as AS
import Data.List
import Streaming as S
import qualified Streaming.ByteString.Char8 as Q
import qualified Streaming.Prelude as S

data Day = Day
  { _code :: String,
    _continent :: String,
    _cases :: Double,
    _deaths :: Double,
    _vaccs :: Double,
    _population :: String
  }
  deriving (Eq, Show)

data GData = GData
  { _name :: String,
    _cont :: String,
    _totcases :: Double,
    _totdeaths :: Double,
    _totvaccs :: Double,
    _popul :: Double
  }
  deriving (Eq, Show)

makeLenses ''Day
makeLenses ''GData

letterNum :: A.Parser Char
letterNum = A.satisfy $ A.inClass "a-zA-Z0-9"

tks :: A.Parser Char
tks = A.satisfy $ A.inClass "-_. '()"

fieldParser :: A.Parser String
fieldParser = A.many' $ letterNum <|> tks

rowParser :: A.Parser [String]
rowParser = A.sepBy' fieldParser (A.char ',')

header :: A.Parser [String]
header = rowParser

rowParser1 :: [Int] -> A.Parser [String]
rowParser1 ins = do
  row <- A.sepBy' fieldParser (A.char ',')
  return $ [row !! i | i <- ins]

csvParser :: [Int] -> A.Parser [Day]
csvParser ins = do
  _ <- rowParser *> A.char '\n'
  res <- A.sepBy' (rowParser1 ins) (A.char '\n')
  return $ map day res

day :: [String] -> Day
day list = Day (head list) (list !! 1) (toDouble $ list !! 2) (toDouble $ list !! 3) (toDouble $ list !! 4) (list !! 5)

toDouble :: [Char] -> Double
toDouble arg = if arg == "" then 0.0 else (read arg :: Double)

getIndexes :: IO [Int]
getIndexes = do
  (res, _) <-
    runResourceT
      . AS.parse header
      $ Q.readFile "owid-covid-data.csv"
  case res of
    Right r -> return $ foldList r
    Left _ -> return []

foldList :: [String] -> [Int]
foldList list = foldr (\y -> if y `elem` cols then (++) (toInt y list) else (++) []) [] list

toInt :: Eq a => a -> [a] -> [Int]
toInt y list = case elemIndex y list of
  Just n -> [n]
  Nothing -> []

cols :: [String]
cols =
  [ "new_cases_smoothed",
    "new_deaths_smoothed",
    "new_vaccinations_smoothed",
    "iso_code",
    "continent",
    "population"
  ]

parseCsv :: [Int] -> IO [Day]
parseCsv ins = do
  (res, _) <-
    runResourceT
      . AS.parse (csvParser ins)
      $ Q.readFile "./owid-covid-data.csv"
  case res of
    Right r -> return $ init r
    Left _ -> return []

codes :: [Day] -> [String]
codes days = nub $ days ^.. folded . code

continents :: [Day] -> [String]
continents days = nub . take 149281 $ tail days ^.. folded . continent

groupByCountries :: [Day] -> IO [GData]
groupByCountries days = do
  res <-
    S.toList
      . S.map (foldr foldCountry (GData "" "" 0.0 0.0 0.0 0.0))
      . S.map (map toCountry)
      . S.mapped S.toList
      . S.groupBy (\a b -> a ^. code == b ^. code)
      $ S.each days
  return $ S.fst' res

toCountry :: Day -> GData
toCountry rec = GData (rec ^. code) (rec ^. continent) (rec ^. cases) (rec ^. deaths) (rec ^. vaccs) (toDouble $ rec ^. population)

foldCountry :: GData -> GData -> GData
foldCountry el1 el2 =
  el1 & totcases .~ (el1 ^. totcases + el2 ^. totcases)
    & totdeaths .~ (el1 ^. totdeaths + el2 ^. totdeaths)
    & totvaccs .~ (el1 ^. totvaccs + el2 ^. totvaccs)

printForContinents :: [GData] -> IO ()
printForContinents cs =
  S.print
    . S.mapped (S.fold foldContinent (GData "" "" 0.0 0.0 0.0 0.0) id)
    . S.groupBy (\a b -> a ^. cont == b ^. cont)
    $ S.each (sortCs cs)

sortCs :: [GData] -> [GData]
sortCs = sortBy (\a b -> compare (a ^. cont) (b ^. cont))

foldContinent :: GData -> GData -> GData
foldContinent el1 el2 =
  el1 & name .~ addCountry (el1 ^. name) (el2 ^. name)
    & totcases .~ (el1 ^. totcases + el2 ^. totcases)
    & totdeaths .~ (el1 ^. totdeaths + el2 ^. totdeaths)
    & totvaccs .~ (el1 ^. totvaccs + el2 ^. totvaccs)
    & popul .~ (el1 ^. popul + el2 ^. popul)
    & cont .~ (el2 ^. cont)

addCountry :: [Char] -> [Char] -> [Char]
addCountry str country_code = if str == "" then country_code else str ++ "," ++ country_code

printForWorld :: [GData] -> IO ()
printForWorld cs = do
  world_data <- S.fold foldContinent (GData "" "" 0.0 0.0 0.0 0.0) id $ S.each cs
  putStrLn "World Data"
  print (S.fst' world_data & cont .~ "")

main :: IO ()
main = do
  ins <- getIndexes
  print ins
  days <- parseCsv ins
  print (toDouble $ head days ^. population)
  countries <- groupByCountries days
  print (length countries)
  putStrLn "Grouped by continents"
  printForContinents countries
  putStrLn "\n\n"
  printForWorld countries

-- import TheLens ()

-- -- Вывести на экран список list
-- -- в cols столбцов (например,
-- -- после каждого числа вывести
-- -- знак табуляции) и вернуть
-- -- сумму элементов списка
-- --
-- -- > sumAndTabulate 3 [1..10]
-- -- >
-- -- >    1       2       3
-- -- >    4       5       6
-- -- >    7       8       9
-- -- >    10
-- -- >
-- -- > 55
-- sumAndTabulate :: Int -> [Int] -> IO Int
-- sumAndTabulate cols list = error "Write me!"

-- -- -- Свободная монада - на одном из следующих занятий
-- -- data S f m r
-- --   = Elem (f (S f m r)) -- "чистый" элемент e и данные дальше
-- --   | Act (m (S f m r))  -- данные из действия (в монаде m)
-- --   | Res r              -- результат r

-- stS :: Int
--     -> Stream (Of Int) IO Int
--     -> IO Int
-- stS cols = fmap S.fst' .
--   S.sum .
--   S.mapM_ (liftIO . putStrLn) .
--   tabS 3 .
--   S.copy

-- -- Получение стрима из нужных строк
-- tabS :: Monad m
--      => Int
--      -> Stream (Of Int) m r
--      -> Stream (Of String) m r
-- tabS cols ints = S.mapsM S.mconcat $
--                  S.chunksOf cols $
--                  S.map addTab ints
--   where addTab x = show x ++ "\t"

-- -- Вывод на экран
-- outTabS :: Int
--         -> Stream (Of Int) IO ()
--         -> IO ()
-- outTabS cols = S.mapM_ putStrLn .
--                tabS cols

-- --------------------------------------

-- type Lens f s t a b
--   = (a -> f b) -- функция модификации поля
--   -> s         -- старый объект
--   -> f t       -- новый объект
-- type Lens' f s a = Lens f s s a a

-- ix :: Functor f => Int -> Lens' f [a] a
-- ix k = go k
--   where
--     go 0 f (x:xs) = (:xs) <$> f x
--     go k f (x:xs) = (x:) <$> go (k-1) f xs

-- _1 :: Functor f => Lens f (a,b) (c,b) a c
-- _1 f (a,b) = (\x -> (x,b)) <$> f a

-- _2 :: Functor f => Lens f (a,b) (a,d) b d
-- _2 f (a,b) = (\x -> (a,x)) <$> f b

-- ex1 = (ix 4) Identity [1..10]
-- ex2 = (ix 4) (const $ Identity 101) [1..10]
-- ex3 = (ix 4) (\x -> [101..104]) [1..10]
-- ex4 = (ix 4) (\x -> (x,x)) [1..10]
-- ex5 = _1 (const $ Identity 789) (123,456)
-- ex6 = _1 (const $ Identity "ads") (123,456)
-- ex7 = _2 (Identity . show) (123,456)
-- ex8 = _2 (\x -> [show x, show $ x+1]) (123,456)
-- ex9 = (_1 . _2) (const $ Identity "x") ((1,2),3)

-- {-------------------

-- * Main> ((1,[3::Int,4,5]),(2,3))^._2._2

-- 3

-- * Main> ((1,[3::Int,4,5]),(2,3)) & _2._2 .~ "asd"

-- ((1,[3,4,5]),(2,"asd"))

-- * Main> ((1,[3::Int,4,5]),(2,3)) & _2._2 .~ (123,321)

-- ((1,[3,4,5]),(2,(123,321)))

-- * Main> ((1,[3::Int,4,5]),(2,3)) ^. _2

-- (2,3)

-- * Main> ((1,[3::Int,4,5]),(2,3)) & _2 %~ (\(a,b) -> (a,b,123))

-- ((1,[3,4,5]),(2,3,123))

-- * Main> ((1,[3::Int,4,5]),(2,3)) ^. _1 . _2

-- [3,4,5]

-- * Main> ((1,[3::Int,4,5]),(2,3)) ^? _1 . _2 . ix 5

-- Nothing

-- * Main> ((1,[3::Int,4,5]),(2,3)) ^? _1 . _2 . ix 1

-- Just 4

-- * Main> ((1,[3::Int,4,5]),(2,3)) ^.. _1 . _2 . ix 1

-- [4]

-- * Main> Right 123 :: Either String Int

-- Right 123

-- * Main> let ea = Right 123 :: Either String Int

-- * Main> ea ^? _Left

-- Nothing

-- * Main> ea ^? _Right

-- Just 123

-- * Main> ea & _Right .~ 124

-- Right 124

-- * Main> ea & _Left .~ 124

-- Right 123

-- * Main> ea & _Left .~ "123"

-- Right 123

-- * Main> ea & _Left %~ ("f" ++)

-- Right 123

-- * Main> let eb = Left "asdasd"

-- * Main> eb & _Left %~ ("f" ++)

-- Left "fasdasd"

-- * Main> ea & _Left %~ ("f" ++)

-- Right 123

-- * Main> ea & _Left %~ ("f" ++)

-- Right 123
-- ------------------}
