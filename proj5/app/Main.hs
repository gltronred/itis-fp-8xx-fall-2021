{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where


import System.Locale as L
import Data.Time.Format as F
import Data.List.Split
import Data.Time.ISO8601
import Data.Time
import Data.Time.Format
import Data.Time.Clock
import Text.Printf
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

data Root = Root
  { rBooks :: String,
    rCharacters :: String,
    rHouses :: String
  } deriving (Show, Generic)

instance FromJSON Root where
  parseJSON = withObject "Root" $ \obj -> do
    books_ <- obj .: "books"
    characters_ <- obj .: "characters"
    houses_ <- obj .: "houses"
    pure $ Root books_ characters_ houses_

data Book = Book
  { url :: String,
    name :: String,
    isbn :: String,
    authors :: [String],
    numberOfPages :: Int,
    publisher :: String,
    country :: String,
    mediaType :: String,
    released :: UTCTime,
    characters :: [String],
    povCharacters :: [String]
  } deriving (Show, Generic)

instance FromJSON Book where
    parseJSON = withObject "Book" $ \obj -> do
      type_ <- obj .: "url"
      name_ <- obj .: "name"
      isbn_ <- obj .: "isbn"
      authors_ <- obj .: "authors"
      numberOfPages_ <- obj .: "numberOfPages"
      publisher_ <- obj .: "publisher"
      country_ <- obj .: "country"
      mediaType_ <- obj .: "mediaType"
      released_ <- obj .: "released"
      characters_ <- obj .: "characters"
      povCharacters_ <- obj .: "povCharacters"
      pure $ Book type_ name_ isbn_ authors_
          numberOfPages_ publisher_ country_ mediaType_
          (isoStringToUTC released_) characters_ povCharacters_

data Gender = Male | Female deriving (Show, Generic)

data Character = Character
  { cUrl :: String,
    cName :: String,
    cGender :: Gender,
    cCulture :: String,
    cBorn :: String,
    cDied :: String,
    cTitles :: [String],
    cAliases :: [String],
    cFather :: String,
    cMother :: String,
    cSpouse :: String,
    cAllegiances :: [String],
    cBooks :: [String],
    cPovBook :: [String],
    cTvSeries :: [String],
    cPlayedBy :: [String]
  } deriving (Show, Generic)

instance FromJSON Character where
  parseJSON = withObject "Character" $ \obj -> do
    url_ <- obj .: "url"
    name_ <- obj .: "name"
    gender_ <- obj .: "gender"
    culture_ <- obj .: "culture"
    born_ <- obj .: "born"
    died_ <- obj .: "died"
    titles_ <- obj .: "titles"
    aliases_ <- obj .: "aliases"
    father_ <- obj .: "father"
    mother_ <- obj .: "mother"
    spouse_ <- obj .: "spouse"
    allegiances_ <- obj .: "allegiances"
    books_ <- obj .: "books"
    povBooks_ <- obj .: "povBooks"
    tvSeries_ <- obj .: "tvSeries"
    playedBy_ <- obj .: "playedBy"

    case (gender_ :: Text) of
      "Female" -> pure $ Character url_ name_ Female culture_ born_ died_ titles_
                    aliases_ father_ mother_ spouse_ allegiances_ books_
                    povBooks_ tvSeries_ playedBy_

      "Male" -> pure $ Character url_ name_ Male culture_ born_ died_ titles_
                    aliases_ father_ mother_ spouse_ allegiances_ books_
                    povBooks_ tvSeries_ playedBy_

data House = House
  { hUrl :: String,
    hName :: String,
    hRegion :: String,
    rCoatOfArms :: String,
    rWords :: String,
    rTitles :: [String],
    rSeats :: [String],
    rCurrentLord :: String,
    rHeir :: String,
    rOverLord :: String,
    rFounded :: String,
    rFounder :: String,
    rDiedOut :: String,
    rAncestralWeapons :: [String],
    rCadetBranches :: [String],
    rSwornMembers :: [String]
  } deriving (Show, Generic)

instance FromJSON House where
  parseJSON = withObject "House" $ \obj -> do
    url_ <- obj .: "url"
    name_ <- obj .: "name"
    region_ <- obj .: "region"
    coatOfArms_ <- obj .: "coatOfArms"
    words_ <- obj .: "words"
    titles_ <- obj .: "titles"
    seats_ <- obj .: "seats"
    currentLord_ <- obj .: "currentLord"
    heir_ <- obj .: "heir"
    overlord_ <- obj .: "overlord"
    founded_ <- obj .: "founded"
    founder_ <- obj .: "founder"
    diedOut_ <- obj .: "diedOut"
    ancestralWeapons_ <- obj .: "ancestralWeapons"
    cadetBranches_ <- obj .: "cadetBranches"
    swornMembers_ <- obj .: "swornMembers"
    pure $ House url_ name_ region_ coatOfArms_ words_ titles_
              seats_ currentLord_ heir_ overlord_ founded_ founder_
              diedOut_ ancestralWeapons_ cadetBranches_ swornMembers_

root :: ClientM Root
book :: Int -> ClientM Book
books :: Maybe String ->  Maybe String ->  Maybe String ->
  Maybe Int -> Maybe Int -> ClientM [Book]
character :: Int -> ClientM Character
fcharacters :: Maybe String -> Maybe String -> Maybe String ->
  Maybe String -> Maybe String -> Maybe Bool -> Maybe Int -> Maybe Int ->
  ClientM [Character]
fhouse :: Int -> ClientM House
fhouses :: Maybe String -> Maybe String -> Maybe String ->
  Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool ->
  Maybe Int -> Maybe Int -> ClientM [House]

type API = "api" :> Get '[JSON] Root
      :<|> "api/books" :> Capture "x" Int  :> Get '[JSON] Book
      :<|> "api/books" :>
        QueryParam "name" String :>
        QueryParam "fromReleaseDate" String :>
        QueryParam "toReleaseDate" String :>
        QueryParam "page" Int :>
        QueryParam "pageSize" Int :>
        Get '[JSON] [Book]
      :<|> "api/characters" :> Capture "x" Int  :> Get '[JSON] Character
      :<|> "api/characters" :>
        QueryParam "name" String :>
        QueryParam "gender" String :>
        QueryParam "culture" String :>
        QueryParam "born" String :>
        QueryParam "died" String :>
        QueryParam "isAlive" Bool :>
        QueryParam "page" Int :>
        QueryParam "pageSize" Int :>
        Get '[JSON] [Character]
      :<|> "api/houses" :> Capture "x" Int  :> Get '[JSON] House
      :<|> "api/houses" :>
          QueryParam "name" String :>
          QueryParam "region" String :>
          QueryParam "words" String :>
          QueryParam "hasWords" Bool :>
          QueryParam "hasTitles" Bool :>
          QueryParam "hasSeats" Bool :>
          QueryParam "hasDiedOut" Bool :>
          QueryParam "hasAncestralWeapons" Bool :>
          QueryParam "page" Int :>
          QueryParam "pageSize" Int :>
          Get '[JSON] [House]

api :: Proxy API
api = Proxy

root :<|> book :<|> books :<|> character :<|>
  fcharacters :<|> fhouse :<|> fhouses = client api

getBook :: Int -> ClientM (Book)
getBook num= do
  b <- book num
  return (b)

getBooks :: Maybe String -> Maybe String -> Maybe String -> Maybe Int -> Maybe Int -> ClientM [Book]
getBooks a b c d e = do
  b <- books a (stringToISO b) (stringToISO c) d e
  return (b)

getRoot :: ClientM (Root)
getRoot = do
  r <- root
  return (r)

getCharacter :: Int -> ClientM Character
getCharacter = do
  c <- character
  return c

getCharacters :: Maybe String -> Maybe Gender -> Maybe String ->
                  Maybe String -> Maybe String -> Maybe Bool ->
                  Maybe Int -> Maybe Int -> ClientM [Character]
getCharacters n (Just g) c b d a p ps = do
  c <- fcharacters n (Just $ show g) c b d a p ps
  return c
getCharacters n Nothing c b d a p ps = do
  c <- fcharacters n Nothing c b d a p ps
  return c

getHouse :: Int -> ClientM House
getHouse = do
  h <- fhouse
  return h

getHouses :: Maybe String -> Maybe String -> Maybe String -> Maybe Bool ->
  Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool ->
  Maybe Int -> Maybe Int -> ClientM [House]
getHouses = do
  h <- fhouses
  return h

stringToUTC :: String -> UTCTime
stringToUTC date = (read (date ++ " UTC")) :: UTCTime

isoStringToUTC :: String -> UTCTime
isoStringToUTC str = case (parseISO8601 (str ++ "Z")) of
                        Nothing -> error "Not correct date"
                        Just a -> a

pop :: [a] -> [a]
pop [] = []
pop xs = init xs

stringToISO :: Maybe String -> Maybe String
stringToISO (Just a) = Just $ pop $ formatISO8601 $ stringToUTC a
stringToISO Nothing = Nothing

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
  rH <- runApi $ do
    x <- getHouses Nothing Nothing Nothing (Just True) (Just True) ( Just True) Nothing Nothing (Just 1) (Just 1)
    y <- getHouse 1
    pure (x, y)
  print rH
  putStrLn "\n"

  rB <- runApi $ do
    b1 <- getBooks Nothing Nothing Nothing (Just 1) (Just 1)
    b2 <- getBook 12
    pure (b1, b2)
  print rB
  putStrLn "\n"

  rC <- runApi $ do
    c1 <- getCharacters (Just "Walder") (Just Male) Nothing Nothing Nothing Nothing (Just 1) (Just 1)
    c2 <- getCharacter 1
    pure (c1, c2)
  print rC
  putStrLn "\n"

  rR <- runApi $ do
    rRoot <- getRoot
    pure rRoot
  print rR
  putStrLn "\n"
