{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.Client
import Web.HttpApiData

data Book = Book
  { bookUrl :: String,
    bookName :: String,
    isbn :: String,
    authors :: [String],
    numberOfPages :: Integer,
    publisher :: String,
    country :: String,
    mediaType :: String,
    released :: String,
    characters :: [String],
    povCharacters :: [String]
  }
  deriving (Show)

instance FromJSON Book where
  parseJSON (Object v) =
    Book
      <$> v .: "url"
      <*> v .: "name"
      <*> v .: "isbn"
      <*> v .: "authors"
      <*> v .: "numberOfPages"
      <*> v .: "publisher"
      <*> v .: "country"
      <*> v .: "mediaType"
      <*> v .: "released"
      <*> v .: "characters"
      <*> v .: "povCharacters"
  parseJSON invalid = typeMismatch "Book" invalid

data Character = Character
  { characterUrl :: String,
    characterName :: String,
    gender :: String,
    culture :: String,
    born :: String,
    died :: String,
    characterTitles :: [String],
    aliases :: [String],
    father :: String,
    mother :: String,
    spouse :: String,
    allegiances :: [String],
    books :: [String],
    povBooks :: [String],
    tvSeries :: [String],
    playedBy :: [String]
  }
  deriving (Show)

instance FromJSON Character where
  parseJSON (Object v) =
    Character
      <$> v .: "url"
      <*> v .: "name"
      <*> v .: "gender"
      <*> v .: "culture"
      <*> v .: "born"
      <*> v .: "died"
      <*> v .: "titles"
      <*> v .: "aliases"
      <*> v .: "father"
      <*> v .: "mother"
      <*> v .: "spouse"
      <*> v .: "allegiances"
      <*> v .: "books"
      <*> v .: "povBooks"
      <*> v .: "tvSeries"
      <*> v .: "playedBy"
  parseJSON invalid = typeMismatch "Character" invalid

data House = House
  { houseUrl :: String,
    houseName :: String,
    region :: String,
    coatOfArms :: String,
    words :: String,
    houseTitles :: [String],
    seats :: [String],
    currentLord :: String,
    heir :: String,
    overlord :: String,
    founded :: String,
    founder :: String,
    diedOut :: String,
    ancestralWeapons :: [String],
    cadetBranches :: [String],
    swornMembers :: [String]
  }
  deriving (Show)

instance FromJSON House where
  parseJSON (Object v) =
    House
      <$> v .: "url"
      <*> v .: "name"
      <*> v .: "region"
      <*> v .: "coatOfArms"
      <*> v .: "words"
      <*> v .: "titles"
      <*> v .: "seats"
      <*> v .: "currentLord"
      <*> v .: "heir"
      <*> v .: "overlord"
      <*> v .: "founded"
      <*> v .: "founder"
      <*> v .: "diedOut"
      <*> v .: "ancestralWeapons"
      <*> v .: "cadetBranches"
      <*> v .: "swornMembers"
  parseJSON invalid = typeMismatch "House" invalid

type Api =
  "books"
    :> Capture "id" Int
    :> Get '[JSON] Book
    :<|> "characters"
    :> Capture "id" Int
    :> Get '[JSON] Character
    :<|> "houses"
    :> Capture "id" Int
    :> Get '[JSON] House
    :<|> "houses"
    :> Get '[JSON] [House]

api :: Proxy Api
api = Proxy

getCharacter :: Int -> ClientM Character
getBook :: Int -> ClientM Book
getHouse :: Int -> ClientM House
getAllHouses :: ClientM [House]
getBook :<|> getCharacter :<|> getHouse :<|> getAllHouses = client api

runApi :: ClientM a -> IO (Either String a)
runApi actions = do
  manager <- newTlsManager
  let env =
        mkClientEnv manager $
          BaseUrl Https "www.anapioficeandfire.com" 443 "/api"
  res <- runClientM actions env
  case res of
    Left err -> print err >> pure (Left $ show err)
    Right a -> pure $ Right a

getFirstCharacterIdOrStandart :: Either String Book -> Int
getFirstCharacterIdOrStandart e = case e of
  Left _ -> 583
  Right b -> read (T.unpack $ last (T.splitOn (T.pack "/") (T.pack (firstCharacterFromBook b))))
  where
    firstCharacterFromBook book = head $ characters book

findHouseWithLord :: String -> [House] -> Maybe House
findHouseWithLord lordLink houses
  | null housesWithLord = Nothing
  | otherwise = Just $ head housesWithLord
  where
    housesWithLord = filter (\house -> currentLord house == lordLink) houses

main :: IO ()
main = do
  bookEither <- runApi $ do
    getBook 1
  putStrLn $ case bookEither of
    Left err -> err
    Right book -> "Book name - " ++ bookName book
  characterEither <- runApi $ do
    getCharacter $ getFirstCharacterIdOrStandart bookEither
  putStrLn $ case characterEither of
    Left err -> err
    Right character -> "First character this book - " ++ characterName character
  housesEither <- runApi $ do
    getAllHouses
  putStrLn $ case housesEither of
    Left err -> err
    Right hos -> case findHouseWithLord
      ( "https://anapioficeandfire.com/api/characters/"
          ++ show (getFirstCharacterIdOrStandart bookEither)
      )
      hos of
      Nothing -> "This character is not lord of any house"
      Just ho -> "This character is lord of " ++ houseName ho ++ " house"