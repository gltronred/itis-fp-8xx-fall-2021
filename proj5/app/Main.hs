{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
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
import Data.Aeson.Types

data Book = Book
    { bookUrl :: String
    , bookName :: String
    , isbn :: String
    , authors :: [String]
    , numberOfPages :: Integer
    , publisher :: String
    , country :: String
    , mediaType :: String
    , released :: String
    , characters :: [String]
    , povCharacters :: [String] 
    } deriving Show

instance FromJSON Book where
    parseJSON (Object v) = Book <$>
                           v .: "url" <*>
                           v .: "name" <*>
                           v .: "isbn" <*>
                           v .: "authors" <*>
                           v .: "numberOfPages" <*>
                           v .: "publisher" <*>
                           v .: "country" <*>
                           v .: "mediaType" <*>
                           v .: "released" <*>
                           v .: "characters" <*>
                           v .: "povCharacters"
    parseJSON invalid    = typeMismatch "Book" invalid


data Character = Character
    { charUrl :: String
    , charName :: String
    , gender :: String
    , culture :: String
    , born :: String
    , died :: String
    , charTitles :: [String]
    , aliases :: [String]
    , father :: String
    , mother :: String
    , spouse :: String
    , allegiances :: [String]
    , books :: [String]
    , povBooks :: [String]
    , tvSeries :: [String]
    , playedBy :: [String]
    } deriving Show

instance FromJSON Character where
    parseJSON (Object v) = Character <$>
                           v .: "url" <*>
                           v .: "name" <*>
                           v .: "gender" <*>
                           v .: "culture" <*>
                           v .: "born" <*>
                           v .: "died" <*>
                           v .: "titles" <*>
                           v .: "aliases" <*>
                           v .: "father" <*>
                           v .: "mother" <*>
                           v .: "spouse" <*>
                           v .: "allegiances" <*>
                           v .: "books" <*>
                           v .: "povBooks" <*>
                           v .: "tvSeries" <*>
                           v .: "playedBy"
    parseJSON invalid    = typeMismatch "Character" invalid

data House = House
    { houseUrl :: String
    , houseName :: String
    , region :: String
    , coatOfArms :: String
    , words :: String
    , houseTitles :: [String]
    , seats :: [String]
    , currentLord :: String
    , heir :: String
    , overlord :: String
    , founded :: String
    , founder :: String
    , diedOut :: String
    , ancestralWeapons :: [String]
    , cadetBranches :: [String]
    , swornMembers :: [String]
    } deriving Show

instance FromJSON House where
    parseJSON (Object v) = House <$>
                           v .: "url" <*>
                           v .: "name" <*>
                           v .: "region" <*>
                           v .: "coatOfArms" <*>
                           v .: "words" <*>
                           v .: "titles" <*>
                           v .: "seats" <*>
                           v .: "currentLord" <*>
                           v .: "heir" <*>
                           v .: "overlord" <*>
                           v .: "founded" <*>
                           v .: "founder" <*>
                           v .: "diedOut" <*>
                           v .: "ancestralWeapons" <*>
                           v .: "cadetBranches" <*>
                           v .: "swornMembers"
    parseJSON invalid    = typeMismatch "House" invalid


type Api = "books" :> QueryParam "name" Text :> QueryParam "fromReleaseDate" Text :> QueryParam "toReleaseDate" Text :> Get '[JSON] [Book] 
  :<|> "books" :> Capture "id" Int :> Get '[JSON] Book
  :<|> "characters" :> QueryParam "name" Text :> QueryParam "gender" Text :> QueryParam "culture" Text :> QueryParam "born" Text 
    :> QueryParam "died" Text :> QueryParam "isAlive" Bool :> Get '[JSON] [Character]
  :<|> "characters" :> Capture "id" Int :> Get '[JSON] Character
  :<|> "houses" :> QueryParam "name" Text :> QueryParam "region" Text :> QueryParam "words" Text :> QueryParam "hasWords" Bool 
    :> QueryParam "hasTitles" Bool :> QueryParam "hasSeats" Bool :> QueryParam "hasDiedOut" Bool :> QueryParam "hasAncestralWeapons" Bool :> Get '[JSON] [House]
  :<|> "houses" :> Capture "id" Int :> Get '[JSON] House

api :: Proxy Api
api = Proxy

getBooks :: Maybe Text-> Maybe Text -> Maybe Text -> ClientM [Book]

getBookById :: Int -> ClientM Book

getCharacters :: Maybe Text-> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> ClientM [Character]

getCharacterById :: Int -> ClientM Character

getHouses :: Maybe Text-> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> ClientM [House]

getHouseById :: Int -> ClientM House

getBooks :<|> getBookById :<|> getCharacters :<|> getCharacterById :<|> getHouses :<|> getHouseById = client api

runApi :: ClientM a -> IO (Either String a)
runApi actions = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr $
        BaseUrl Https "www.anapioficeandfire.com" 443 "/api"
  res <- runClientM actions env
  case res of
    Left err -> print err >> pure (Left $ show err)
    Right a -> pure $ Right a


baseUrl :: String
baseUrl = "http://www.anapioficeandfire.com/api"



main :: IO ()
main = do
  putStrLn $ "\n\n\n"
  r1 <- runApi $ do
    x1 <- getBooks Nothing (Just "2005-02-01T00:00:00") Nothing
    x2 <- getBookById 1
    pure (x1,x2)
  print $ r1
  putStrLn $ "\n\n\n"
  r2 <- runApi $ do
    y1 <- getCharacters (Just "Jon Snow") Nothing Nothing (Just "In 283 AC") Nothing Nothing
    y2 <- getCharacterById 324
    pure (y1,y2)
  print $ r2
  putStrLn $ "\n\n\n"
  r3 <- runApi $ do
    z1 <- getHouses Nothing Nothing Nothing Nothing Nothing (Just True) Nothing (Just True)
    z2 <- getHouseById 34
    pure (z1,z2)
  print $ r3
  putStrLn $ "\n\n\n"
