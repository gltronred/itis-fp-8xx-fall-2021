{-# LANGUAGE TemplateHaskell #-}

module Main where

import Streaming
import qualified Streaming.Prelude as S
import qualified Streaming.ByteString.Char8 as Q
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Streaming as AS
import Data.Function ((&), on)
import Text.Read (readMaybe)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Attoparsec.ByteString.Char8 (isEndOfLine, isHorizontalSpace)
import Control.Lens
import Control.Lens.TH
import Control.Foldl as L

main :: IO ()
main = putStrLn "Hello, Haskell!"


data CovidRecord = CovidRecord {
  _newCasesSmoothed :: Maybe Double,
  _newDeathsSmoothed :: Maybe Double,
  _newVaccinationsSmoothed :: Maybe Double,
  _isoCode :: String,
  _continent :: String,
  _population :: Integer
} deriving (Eq, Show)
makeLenses ''CovidRecord

data CovidRecordByGroup = CovidRecordByGroup {
  _casesSmoothed :: Double,
  _deathsSmoothed :: Double,
  _vaccinationsSmoothed :: Double,
  _name :: String,
  _placePopulation :: Integer
}
makeLenses ''CovidRecordByGroup


parseCsvRow row = if Prelude.length row < 49 then Nothing else Just $ parseCsvRow' row

parseCsvRow' row = CovidRecord (readFieldMaybe 6) (readFieldMaybe 9) (readFieldMaybe 39) (row !! 0) (row !! 1) (floor $ read $ row !! 48) 
   where 
     readFieldMaybe index = readMaybe $ row !! index

rowPraser :: A.Parser [String]
rowPraser = cellParser `A.sepBy'` (A.char ',')

cellParser :: A.Parser String
cellParser = A.many' $ A.satisfy (A.notInClass ",\n")

recordParser = A.skipWhile (/='\n') >> A.char '\n' >> (A.option Nothing (parseCsvRow <$> rowPraser)) `A.sepBy'` (A.char '\n')

unpackMaybe (Just r) = [r]
unpackMaybe Nothing = []

readRecords :: String -> IO [CovidRecord]
readRecords path = do 
  (res,_) <- runResourceT 
    . AS.parse recordParser 
    $ Q.readFile path
  case res of
    Right r -> return (r >>= unpackMaybe)
    Left _ -> return []

orElse replacement (Just x) = x
orElse replacement (Nothing) = replacement

sumGroups path = do 
  records <- readRecords path
  return $ (S.print . S.each $ records) 

sumAndCastMaybe ma mb = (+) <$> ma <*> mb & orElse 0

sumFields field = sumAndCastMaybe `on` (^. field)

sumByPlace r r' = CovidRecordByGroup cases deaths vaccinations name' population' where 
  cases = sumFields newCasesSmoothed r r'
  deaths = sumFields newDeathsSmoothed r r'
  vaccinations = sumFields newVaccinationsSmoothed r r'
  name' = r ^. isoCode
  population' = r ^. population
