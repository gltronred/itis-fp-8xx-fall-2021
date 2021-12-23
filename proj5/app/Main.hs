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

runApi :: ClientM a -> IO (Either String a)
runApi actions = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr $
        BaseUrl Https "anapioficeandfire.com" 443 ""
  res <- runClientM actions env
  case res of
    Left err -> print err >> pure (Left $ show err)
    Right a -> pure $ Right a

type BookApi = "api/books"
  :> Capture "id" Int
  :> Get '[JSON] Book

api :: Proxy BookApi
api = Proxy

main :: IO ()
main = do
  res <- runApi $ do
    id <- return 1
    book <- client api id
    return book
  print res

data Book = Book { url :: String, name :: String, isbn :: String, authors :: [String], numberOfPages :: Int, publisher :: String,
  country :: String, mediaType :: String, released :: String, characters :: [String], povCharacters :: [String]}
    deriving (Generic, Eq, Show)

instance FromJSON Book where
  parseJSON = withObject "Book" $ \x -> do
    url <- x .: "url"
    name <- x .: "name"
    isbn <- x .: "isbn"
    authors <- x .: "authors"
    numberOfPages <- x .: "numberOfPages"
    publisher <- x .: "publisher"
    country <- x .: "country"
    mediaType <- x .: "mediaType"
    released <- x .: "released"
    characters <- x .: "characters"
    povCharacters <- x .: "povCharacters"
    pure $ Book url name isbn authors numberOfPages publisher country mediaType released characters povCharacters