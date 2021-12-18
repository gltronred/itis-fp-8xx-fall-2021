module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

type Parser = Parsec Void String

newLineSymbol :: Char
newLineSymbol = '\n'

commaSymbol :: Char
commaSymbol = ','

getValueParser :: String -> Parser String
getValueParser separator = many ((char '\\' >> anySingle) <|> noneOf separator)

parseValue :: Parser String
parseValue = getValueParser [commaSymbol, newLineSymbol]

parseQuotedValue :: Parser String
parseQuotedValue = do
  _ <- char '"'
  res <- getValueParser "\""
  _ <- char '"'
  return res

alternateValue = parseQuotedValue <|> parseValue

row :: Parser [String]
row = sepBy alternateValue (single commaSymbol)

csvParser :: Parser [[String]]
csvParser = sepBy row (single newLineSymbol)

csv :: String
csv = "col1,col2,col3\nr2 c1,r2 c2,r2 c3\n\"r3,c1\",\"r3,c2\",\"r3,c3\"\n\"r4\\\",\\\"\\\\c1\",\"r4\\\",\\\"c2\",\"r4\\\",\\\"c3\""

csvRes :: [[String]]
csvRes =
  [ ["col1", "col2", "col3"],
    ["r2 c1", "r2 c2", "r2 c3"],
    ["r3,c1", "r3,c2", "r3,c3"],
    ["r4\",\"\\c1", "r4\",\"c2", "r4\",\"c3"]
  ]

main :: IO ()
main = do
  case parse csvParser "" csv of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right xs -> print xs
  print csvRes