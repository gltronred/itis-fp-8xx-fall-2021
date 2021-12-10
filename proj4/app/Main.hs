{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List
import Data.Maybe
import Control.Monad.Trans.Resource.Internal
import Streaming as S
import qualified Streaming.Prelude as S
import Control.Monad.Trans.Resource (runResourceT)
import qualified Streaming.ByteString.Char8 as Q
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Streaming as AS
import Control.Lens
import Control.Lens.TH

data Country = Country
  { _code :: String
  , _continent :: String
  , _population :: Int
  } deriving (Eq,Show)

data Covid = Covid
  { _cases :: Double
  , _deaths :: Double
  , _vaccinations :: Double
  } deriving (Eq,Show)

data CovDay = CovDay
  { _countryD :: Country   
  , _covidD :: Covid
  } deriving (Eq,Show)

data CovCountry = CovCountry
  { _countryCou :: Country
  , _covidCou :: Covid
  } deriving (Eq,Show)

data CovContinent = CovContinent
  { _name :: String
  , _populationCont :: Int
  , _covidCont :: Covid 
  } deriving (Eq,Show)

data CovGlobal = CovGlobal
  { _populationG :: Int
  , _covidG :: Covid
  } deriving (Eq,Show)

makeLenses ''Covid
makeLenses ''Country
makeLenses ''CovDay
makeLenses ''CovCountry
makeLenses ''CovContinent

csvParser :: A.Parser [[String]]
csvParser = A.sepBy' rowPraser (A.char '\n')

rowPraser :: A.Parser [String]
rowPraser = A.sepBy' (quotedValueParser <|> valueParser) (A.char ',')

valueParser :: A.Parser String
valueParser = A.many' $ (A.char '\\' >> A.anyChar) <|> A.satisfy (A.notInClass ",\n")

quotedValueParser :: A.Parser String
quotedValueParser = do
    _ <- A.char '"'
    result <- A.many' $ (A.char '\\' >> A.anyChar) <|> A.satisfy (A.notInClass "\"")
    _ <- A.char '"'
    return result

parseCsv :: String -> IO [[String]]
parseCsv path = do 
  (res,_) <- runResourceT 
    . AS.parse csvParser 
    $ Q.readFile path
  case res of
    Right r -> return $ init r
    Left _ -> return []

toCovDays :: [[String]] -> IO [CovDay]
toCovDays (headers:csv) = do
  let caseIndex = fromJust $ elemIndex "new_cases_smoothed" headers
  let deathIndex = fromJust $ elemIndex "new_deaths_smoothed" headers
  let vacIndex = fromJust $ elemIndex "new_vaccinations_smoothed" headers
  let isoIndex = fromJust $ elemIndex "iso_code" headers
  let contIndex = fromJust $ elemIndex "continent" headers
  let popIndex = fromJust $ elemIndex "population" headers
  return $ map (\row -> CovDay (Country (row !! isoIndex) (row !! contIndex) (round $ emptyOrRead $ row !! popIndex)) (Covid (emptyOrRead $ row !! caseIndex) (emptyOrRead $ row !! deathIndex) (emptyOrRead $ row !! vacIndex))) $ init csv
    where emptyOrRead s = if s == "" then 0.0 else read s

toCovCountries :: [CovDay] -> IO [CovCountry]
toCovCountries [] = return []
toCovCountries xs = do
  let isos = nub $ xs ^.. folded . countryD . code
  makeCountry xs isos
    where 
      
      makeCountry :: [CovDay] -> [String] -> IO [CovCountry]
      makeCountry xs [] = return []
      makeCountry xs (iso:isos) = do
        let xs' = xs ^.. folded . filtered (codeEquals iso)
        if null xs' then return []
        else do
          let c = CovCountry (head xs' ^. countryD) (Covid (sum $ xs' ^.. folded . covidD . cases) (sum $ xs' ^.. folded . covidD . deaths) (sum $ xs' ^.. folded . covidD . vaccinations))
          cs <- makeCountry xs isos
          return $ c : cs
        
      codeEquals iso x = (x ^. countryD . code) == iso

toCovContinents :: [CovCountry] -> IO [CovContinent]
toCovContinents [] = return []
toCovContinents xs = do
  let names = nub $ xs ^.. folded . countryCou . continent
  makeContinent xs names
    where 
      
      makeContinent :: [CovCountry] -> [String] -> IO [CovContinent]
      makeContinent xs [] = return []
      makeContinent xs (name:names) = do
        let xs' = xs ^.. folded . filtered (continentEquals name)
        if null xs' then return []
        else do
          let c = CovContinent (head xs' ^. countryCou . continent) (sum $ xs' ^.. folded . countryCou . population) (Covid (sum $ xs' ^.. folded . covidCou . cases) (sum $ xs' ^.. folded . covidCou . deaths) (sum $ xs' ^.. folded . covidCou . vaccinations))
          cs <- makeContinent xs names
          return $ c : cs
        
      continentEquals name x = (x ^. countryCou . continent) == name

toCovGlobal :: [CovContinent] -> IO CovGlobal
toCovGlobal [] = return $ CovGlobal 0 (Covid 0 0 0)
toCovGlobal xs = do
  let g = CovGlobal (sum $ xs ^.. folded . populationCont) (Covid (sum $ xs ^.. folded . covidCont . cases) (sum $ xs ^.. folded . covidCont . deaths) (sum $ xs ^.. folded . covidCont . vaccinations))
  return g

main :: IO ()
main = do
  csv <- parseCsv ".\\app\\data\\owid-covid-data.csv"
  days <- toCovDays csv
  countries <- toCovCountries days
  continents <- toCovContinents countries
  print continents
  global <-toCovGlobal continents
  print global