module Main where

import Text.Megaparsec

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

main :: IO ()
main = putStrLn "Hello, Haskell!"
