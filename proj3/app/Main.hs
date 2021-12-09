module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

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
    user <- some alphaNumChar <?> "username"
    _ <- single ':'
    pass <- some alphaNumChar <?> "password"
    _ <- single '@'
    pure (user, pass)
  host <- some (alphaNumChar <|> single '.') <?> "hostname"
  -- mport <- optional $ do
  --   _ <- single ':'
  --   read <$> some digitChar
  mport <- optional $
    single ':' *>
    fmap read (some digitChar <?> "port")
  pure $ Domain mcreds host mport

urlParser :: Parser URL
urlParser = URL
  <$> (schemeParser <?> "valid scheme")
  <*> optional domainParser
  <*  optional (single '/')
  <*> takeRest

-- ((()))
-- ""
bracketParser :: Parser String
bracketParser
  =   ((\c s d -> [c]++s++[d])
       <$> char '('
       <*> bracketParser
       <*> char ')')
  <|> string ""

-- <tag1>
--   <tag2></tag2>
-- </tag1>
-- <tag3></tag3>
type XML = [TagTree]
data TagTree = Tree String (Maybe XML)
  deriving (Eq,Show)
-- [ Tree "tag1" (Just $ Tree "tag2" Nothing)
-- , Tree "tag3" Nothing]

openParser :: Parser String
openParser = char '<' *> some alphaNumChar <* char '>'

closeParser :: String -> Parser String
closeParser tag = string "</" *> string tag <* char '>'

treeParser :: Parser TagTree
treeParser = do
  tag <- openParser
  inner <- optional $ try xmlParser
  closeParser tag
  pure $ Tree tag inner

xmlParser :: Parser XML
xmlParser = many $ try treeParser

csv :: String
csv = "col1,col2,col3\nr2 c1,r2 c2,r2 c3\n\"r3,c1\",\"r3,c2\",\"r3,c3\"\n\"r4\\\",\\\"\\\\c1\",\"r4\\\",\\\"c2\",\"r4\\\",\\\"c3\""

csvRes :: [[String]]
csvRes =
  [ [ "col1", "col2", "col3" ]
  , [ "r2 c1", "r2 c2", "r2 c3" ]
  , [ "r3,c1", "r3,c2", "r3,c3" ]
  , [ "r4\",\"\\c1", "r4\",\"c2", "r4\",\"c3"]
  ]

csvParser :: Parser [[String]]
csvParser = sepEndBy lineParser (char '\n')

-- Одна строка
lineParser :: Parser [String]
lineParser = sepBy1 cellParser (char ',')

-- Значение внутри ячейки в кавычках или без
cellParser :: Parser String
cellParser = quotedCell <|> normalCell

-- Экранированный символ
escapedToken:: Parser (Token String)
escapedToken = char '\\' >> anySingle

normalCell :: Parser String
normalCell = many (escapedToken <|> noneOf ",\n")

quotedCell :: Parser String
quotedCell = char '"' *> many (escapedToken <|> noneOf "\"") <* char '"'

main :: IO ()
main = do
  parseTest csvParser csv