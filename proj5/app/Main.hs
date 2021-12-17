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


data Resource
  = Books
  | Characters
  | Houses
  deriving (Generic,Eq,Show,Read,Bounded,Enum)

instance FromHttpApiData Resource where
  parseUrlPiece = parseBoundedTextData
instance ToHttpApiData Resource where
  toUrlPiece = T.pack . show
instance FromJSON Resource
instance ToJSON Resource where
  toEncoding = genericToEncoding defaultOptions

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
  } deriving (Generic,Eq,Show)
instance FromJSON Book

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
  } deriving (Generic,Eq,Show)
instance FromJSON Character

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
  } deriving (Generic,Eq,Show)
instance FromJSON House

type Api
  = "api" 
  :> Get '[JSON] Value
  :<|> "api"
  :> Capture "resource" Resource
  :> QueryParam "page" Int
  :> QueryParam "pageSize" Int
  :> Get '[JSON] Value
  :<|> "api"
  :> Capture "resource" Resource
  :> Capture "id" Int
  :> Get '[JSON] Value
  

api :: Proxy Api
api = Proxy

getRoot :: ClientM Value

getPaginated :: Resource 
     -> Maybe Int
     -> Maybe Int
     -> ClientM Value

getById :: Resource 
     -> Int
     -> ClientM Value
getRoot :<|> getPaginated :<|> getById = client api

runApi :: ClientM a -> IO (Either String a)
runApi actions = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr $
        BaseUrl Https "anapioficeandfire.com" 443 ""
  res <- runClientM actions env
  case res of
    Left err -> print err >> pure (Left $ show err)
    Right a -> pure $ Right a

main :: IO ()
main = do
  r <- runApi  getRoot
  print r

  cs <- runApi $ getPaginated Characters (Just 2) (Just 3)
  print cs

  c <- runApi $ getById Characters 3
  print c

  cs <- runApi $ getPaginated Books Nothing (Just 4)
  print cs

  c <- runApi $ getById Books 2
  print c

  cs <- runApi $ getPaginated Houses (Just 5) Nothing
  print cs

  c <- runApi $ getById Houses 5
  print c