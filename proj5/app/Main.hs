{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API
import Servant.Client
import Web.HttpApiData

newtype CommaSep a = CommaSep [a]
  deriving (Eq,Show)

instance FromHttpApiData a => FromHttpApiData (CommaSep a) where
  parseUrlPiece t = fmap CommaSep $
                    mapM parseUrlPiece $
                    T.splitOn "," t
  parseQueryParam = fmap CommaSep .
                    mapM parseUrlPiece .
                    T.splitOn ","
instance ToHttpApiData a => ToHttpApiData (CommaSep a) where
  toUrlPiece (CommaSep ts) = T.intercalate "," $ map toUrlPiece ts
  toQueryParam (CommaSep ts) = T.intercalate "," $ map toQueryParam ts

newtype EmptyAny a = EmptyAny (CommaSep a)
  deriving (Eq,Show)

instance FromHttpApiData a => FromHttpApiData (EmptyAny a) where
  parseUrlPiece t = case t of
    "Any" -> pure $ EmptyAny (CommaSep [])
    _ -> EmptyAny <$> parseUrlPiece t
instance ToHttpApiData a => ToHttpApiData (EmptyAny a) where
  toUrlPiece (EmptyAny (CommaSep [])) = "Any"
  toUrlPiece (EmptyAny t) = toUrlPiece t

data Category
  = Programming
  | Misc
  | Dark
  | Pun
  | Spooky
  | Christmas
  deriving (Eq,Show,Read,Bounded,Enum)

instance FromHttpApiData Category where
  parseUrlPiece = parseBoundedTextData
instance ToHttpApiData Category where
  toUrlPiece = T.pack . show

type Categories = EmptyAny Category

data Lang = Cs | De | En | Es | Fr | Pt
  deriving (Eq,Show,Read,Bounded,Enum)

instance FromHttpApiData Lang where
  parseUrlPiece = parseBoundedTextData
instance ToHttpApiData Lang where
  toUrlPiece = T.toLower . T.pack . show

data Flag
  = Nsfw
  | Religious
  | Political
  | Racist
  | Sexist
  | Explicit
  deriving (Eq,Show,Read,Bounded,Enum)

instance FromHttpApiData Flag where
  parseUrlPiece = parseBoundedTextData
instance ToHttpApiData Flag where
  toUrlPiece = T.toLower . T.pack . show

type Flags = CommaSep Flag

data JokeType = Single | Twopart
  deriving (Eq,Show,Read,Bounded,Enum)

instance FromHttpApiData JokeType where
  parseUrlPiece = parseBoundedTextData
instance ToHttpApiData JokeType where
  toUrlPiece = T.toLower . T.pack . show

data Range = Range
  { from :: Int
  , to :: Int
  }
  deriving (Eq,Show)

instance FromHttpApiData Range where
  parseQueryParam t = do
    let ts = T.splitOn "-" t
    ints <- mapM parseQueryParam ts
    case ints of
      [f,t] -> pure $ Range f t
      _ -> Left "Wrong number of components in range"
instance ToHttpApiData Range where
  toQueryParam (Range f t) = T.concat
    [ T.pack $ show f, "-", T.pack $ show t]

type JokeApi
  = "joke"
  :> Capture "category" Categories
  :> QueryParam "lang" Lang
  :> QueryParam "blacklistFlags" Flags
  :> QueryParam "type" JokeType
  :> QueryParam "contains" Text
  :> QueryParam "idRange" Range
  :> Get '[JSON] Value

api :: Proxy JokeApi
api = Proxy

joke :: Categories
     -> Maybe Lang
     -> Maybe Flags
     -> Maybe JokeType
     -> Maybe Text
     -> Maybe Range
     -> ClientM Value
joke = client api

run :: ClientM a -> IO (Either String a)
run actions = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr $
        BaseUrl Https "v2.jokeapi.dev" 443 ""
  res <- runClientM actions env
  case res of
    Left err -> print err >> pure (Left $ show err)
    Right a -> pure $ Right a

main :: IO ()
main = do
  r <- run $ do
    x <- joke (EmptyAny (CommaSep [])) Nothing Nothing Nothing Nothing Nothing
    y <- joke (EmptyAny (CommaSep [Programming])) Nothing Nothing Nothing Nothing Nothing
    z <- joke (EmptyAny (CommaSep [])) (Just De) Nothing Nothing Nothing Nothing
    pure [x,y,z]
  print r
