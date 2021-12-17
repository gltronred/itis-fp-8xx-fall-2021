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
  k <- runIceAndFireApi $ do
    x <- getCharacter 583
    y <- getCharacter 584
    z <- getCharacter 580
    pure [x, y, z]
  print k

runIceAndFireApi :: ClientM a -> IO (Either String a)
runIceAndFireApi actions = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr $
        BaseUrl Https "anapioficeandfire.com" 443 ""
  res <- runClientM actions env
  case res of
    Left err -> print err >> pure (Left $ show err)
    Right a -> pure $ Right a
    
type IceAndFireApi
  = "api/characters"
  :> Capture "id" Int
  :> Get '[JSON] Character

api2 :: Proxy IceAndFireApi
api2 = Proxy

getCharacter :: Int -> ClientM Character
getCharacter = client api2

data Character = Character
  { url :: String
  , name :: String
  , gender :: Gender
  , culture :: String
  , born :: String
  , died :: String
  , titles :: [String]
  , aliases :: [String]
  , father :: String
  , mother :: String
  , spouse :: String
  , allegiances :: [String]
  , books :: [String]
  , povBooks :: [String]
  , tvSeries :: [String]
  , playedBy :: [String]
  }
  deriving (Generic,Eq,Show)
instance FromJSON Character where
  parseJSON = withObject "Character" $ \obj -> do
    url <- obj .: "url"
    name <- obj .: "name"
    gender <- obj .: "gender"
    culture <- obj .: "culture"
    born <- obj .: "born"
    died <- obj .: "died"
    titles <- obj .: "titles"
    aliases <- obj .: "aliases"
    father <- obj .: "father"
    mother <- obj .: "mother"
    spouse <- obj .: "spouse"
    allegiances <- obj .: "allegiances"
    books <- obj .: "books"
    povBooks <- obj .: "povBooks"
    tvSeries <- obj .: "tvSeries"
    playedBy <- obj .: "playedBy"
    pure $ Character url name gender culture born died titles aliases father mother spouse allegiances books povBooks tvSeries playedBy

data Gender = Male | Female
  deriving (Generic,Eq,Show,Read,Bounded,Enum)
instance FromJSON Gender