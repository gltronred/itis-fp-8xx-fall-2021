module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

-- scheme:[//[user:password@]host[:port]][/]path

data URL = URL
  { scheme :: Scheme
  , domain :: Maybe Domain
  , path :: String
  } deriving (Eq,Show)

data Domain = Domain
  { credentials :: Maybe (String, String)
  , host :: String
  , port :: Maybe Int
  } deriving (Eq,Show)

data Scheme
  = SchemeHttp
  | SchemeHttps
  | SchemeData
  deriving (Eq,Show)

-- http://example.com
-- data://asdasdasdads
-- https://user:pass@example.com:1234/asd

schemeParser :: Parser Scheme
schemeParser
  =   SchemeHttps <$ string "https:"
  <|> SchemeHttp  <$ string "http:"
  <|> SchemeData  <$ string "data:"

domainParser :: Parser Domain
domainParser = do
  _ <- string "//"
  mcreds <- optional $ try $ do
    user <- some alphaNumChar
    _ <- single ':'
    pass <- some alphaNumChar
    _ <- single '@'
    pure (user, pass)
  host <- some (alphaNumChar <|> single '.')
  -- mport <- optional $ do
  --   _ <- single ':'
  --   read <$> some digitChar
  mport <- optional $
    single ':' *>
    fmap read (some digitChar)
  pure $ Domain mcreds host mport

urlParser :: Parser URL
urlParser = URL
  <$> schemeParser
  <*> optional domainParser
  <*  optional (single '/')
  <*> takeRest

main :: IO ()
main = putStrLn "Hello, Haskell!"
