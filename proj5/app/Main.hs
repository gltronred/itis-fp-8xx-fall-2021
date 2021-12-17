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
import Data.Aeson.Types (unexpected)

main :: IO ()
main = do
  k <- runApi $ do
    book1 <- getBook 1
    book2 <- getBook 2
    pure [book1, book2]
  print k

runApi :: ClientM a -> IO (Either String a)
runApi actions = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr $
        BaseUrl Https "anapioficeandfire.com" 443 ""
  res <- runClientM actions env
  case res of
    Left err -> print err >> pure (Left $ show err)
    Right a -> pure $ Right a
    
type Api
  = "api/books"
  :> Capture "id" Int
  :> Get '[JSON] Book

api :: Proxy Api
api = Proxy

getBook :: Int -> ClientM Book
getBook = client api

data Book = Book
  { url :: String
  , name :: String
  , isbn :: String
  , authors :: [String]
  , numberOfPages :: Int
  , publisher :: String
  , country :: String
  , mediaType :: String
  , released :: String
  , characters :: [String]
  , povCharacters :: [String]
  }
  deriving (Generic,Eq,Show)

instance FromJSON Book where
  parseJSON = withObject "Book" $ \obj -> do
    url <- obj .: "url"
    name <- obj .: "name"
    isbn <- obj .: "isbn"
    authors <- obj .: "authors"
    numberOfPages <- obj .: "numberOfPages"
    publisher <- obj .: "publisher"
    country <- obj .: "country"
    mediaType <- obj .: "mediaType"
    released <- obj .: "released"
    characters <- obj .: "characters"
    povCharacters <- obj .: "povCharacters"
    pure $ Book url name isbn authors numberOfPages publisher country mediaType released characters povCharacters