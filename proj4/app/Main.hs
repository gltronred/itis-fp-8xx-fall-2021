
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Streaming as S
import qualified Streaming.Prelude as S

import Control.Monad.Trans.Resource (runResourceT)
import qualified Streaming.ByteString.Char8 as Q
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Streaming as AS

import Data.List
import Control.Lens


data Day 
  = Day 
  { _code :: String
  , _continent :: String
  , _cases :: Double
  , _deaths :: Double
  , _vaccs :: Double
  , _population :: String 
  } deriving (Eq,Show)

data GData 
  = GData
  { _name :: String
  , _cont :: String
  , _totcases :: Double
  , _totdeaths :: Double
  , _totvaccs :: Double
  , _popul :: Double 
  } deriving (Eq,Show)

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
  return $ [ row !! i | i <- ins]

csvParser :: [Int] -> A.Parser [Day]
csvParser ins = do 
  _ <- rowParser *> (A.char '\n')
  res <- A.sepBy' (rowParser1 ins) (A.char '\n')
  return $ map (\x -> day x) res

day :: [String] -> Day
day list = Day (list !! 0) (list !! 1) (toDouble $ list !! 2) (toDouble $ list !! 3) (toDouble $ list !! 4) (list !! 5) 

toDouble :: [Char] -> Double
toDouble arg = if arg == "" then 0.0 else (read arg :: Double)


getIndexes :: IO [Int]
getIndexes = do 
  (res,_) <- runResourceT 
    . AS.parse (header) 
    $ Q.readFile "./owid-covid-data.csv"
  case res of
    Right r -> return $ foldList r
    Left _ -> return []

foldList :: [String] -> [Int]
foldList list = foldr (\y -> if elem y cols then (++) (toInt y list) else (++) []) [] list

toInt :: Eq a => a -> [a] -> [Int]
toInt y list = case elemIndex y list of
  Just n -> [n]
  Nothing -> []



cols :: [String]
cols = ["iso_code","continent","new_cases_smoothed","new_deaths_smoothed","new_vaccinations_smoothed","population"]

parseCsv :: [Int] -> IO [Day]
parseCsv ins = do 
  (res,_) <- runResourceT 
    . AS.parse (csvParser ins) 
    $ Q.readFile "./owid-covid-data.csv"
  case res of
    Right r -> return $ init r
    Left _ -> return []


codes :: [Day] -> [String]
codes days = nub $ days ^.. folded . code

continents :: [Day] -> [String]
continents days = nub . take 137600 $ (tail days) ^.. folded . continent



groupByCountries :: [Day] -> IO [GData]
groupByCountries days = do 
  res <- S.toList
    . S.map (\x -> foldr foldCountry (GData "" "" 0.0 0.0 0.0 0.0) x)
    . S.map (\x -> map toCountry x)
    . S.mapped S.toList
    . S.groupBy (\a b -> a ^. code == b ^. code) 
    . S.take 137603
    $ S.each days
  return $ S.fst' res

toCountry :: Day -> GData
toCountry rec = GData (rec ^. code) (rec ^. continent) (rec ^. cases) (rec ^. deaths) (rec ^. vaccs) (toDouble $ rec ^. population)

foldCountry :: GData -> GData -> GData
foldCountry el1 el2 = el1 & totcases .~ (el1 ^. totcases + el2 ^. totcases) 
  & totdeaths .~ (el1 ^. totdeaths + el2 ^. totdeaths) 
  & totvaccs .~ (el1 ^. totvaccs + el2 ^. totvaccs)



printForContinents :: [GData] -> IO ()
printForContinents cs = S.print
  . S.mapped (S.fold foldContinent (GData "" "" 0.0 0.0 0.0 0.0) id) 
  . S.groupBy (\a b -> a ^. cont == b ^. cont)
  $ S.each (sortCs cs)

sortCs :: [GData] -> [GData]
sortCs cs = sortBy (\a b -> compare (a ^. cont) (b ^. cont)) cs

foldContinent :: GData -> GData -> GData
foldContinent el1 el2 = el1 & name .~ (addCountry (el1 ^. name) (el2 ^. name))
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
  putStrLn $ show $ (S.fst' world_data) & cont .~ ""


main :: IO ()
main = do
  ins <- getIndexes
  putStrLn $ show ins
  days <- parseCsv ins
  putStrLn $ show $ toDouble $ (head days) ^. population
  countries <- groupByCountries days
  putStrLn $ show $ length $ countries
  -- putStrLn $ show $ take 11 $ countries
  putStrLn "Grouped by continents"
  printForContinents countries
  putStrLn "\n\n"
  printForWorld countries

  
