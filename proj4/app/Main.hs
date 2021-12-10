module Main where

import Streaming as S

import Data.List
import Control.Monad.Trans.Resource (runResourceT)
import qualified Streaming.Prelude as S
import qualified Data.ByteString.Streaming.Char8 as Q
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Streaming as AS

import TheLens

main :: IO ()
main = do
  file <- parseFile
  print file

data Day = Day
  {
    _new_cases_smoothed :: Double,
    _new_deaths_smoothed :: Double,
    _new_vaccinations_smoothed :: Double,
    _iso_code :: String,
    _continent :: String,
    _population :: Int
  } deriving (Eq,Show)

day :: [String] -> Day
day xs = Day (toDouble $ head xs) (toDouble $ xs !! 1) (toDouble $ xs !! 2) (xs !! 3) (xs !! 4) (toInt $ xs !! 5)
  where 
    toDouble arg = if arg == "" then 0.0 else (read arg :: Double)
    toInt arg = round $ toDouble arg

parseField :: A.Parser String
parseField = A.many' $ A.satisfy (A.inClass "a-zA-Z0-9") <|> A.satisfy (A.inClass "-_. '()")

parseHeader :: A.Parser [String]
parseHeader = A.sepBy' parseField (A.char ',')

parseRows :: [Int] -> A.Parser [String]
parseRows ins = do
  row <- parseHeader
  return $ [ row !! i | i <- ins]

-- "new_cases_smoothed", "new_deaths_smoothed", "new_vaccinations_smoothed", "iso_code", "continent", "population"
columnIndices :: [Int]
columnIndices = [6, 9, 39, 0, 1, 48]

parseCsv :: [Int] -> A.Parser [Day]
parseCsv indices = do
  _ <- parseHeader *> A.char '\n'
  res <- A.sepBy' (parseRows indices) (A.char '\n')
  return $ map day res

parseFile :: IO [Day]
parseFile = do
  (res,_) <- runResourceT . AS.parse (parseCsv columnIndices) $ Q.readFile "./app/owid-covid-data.csv"
  case res of
    Right r -> return $ init r
    Left _ -> return []