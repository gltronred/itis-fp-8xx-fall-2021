module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

type Parser = Parsec Void String

-- Инпут
csv :: String
csv = "col1,col2,col3\nr2 c1,r2 c2,r2 c3\n\"r3,c1\",\"r3,c2\",\"r3,c3\"\n\"r4\\\",\\\"\\\\c1\",\"r4\\\",\\\"c2\",\"r4\\\",\\\"c3\""
-- Аутпут
csvRes :: [[String]]
csvRes =
  [ [ "col1", "col2", "col3" ]
  , [ "r2 c1", "r2 c2", "r2 c3" ]
  , [ "r3,c1", "r3,c2", "r3,c3" ]
  , [ "r4\",\"\\c1", "r4\",\"c2", "r4\",\"c3"]
  ]


-- Парсер
csvParser :: Parser [[String]]
csvParser = sepEndBy row (char '\n')

-- Одна строка
row :: Parser [String]
row = sepBy1 value (char ',') -- sepBy1 чтобы не было пустых строк

-- Значение: либо обычное, либо в кавычках
value :: Parser String
value = quotedValue <|> normalValue

-- Значение обычное
normalValue :: Parser String
normalValue = many ((char '\\' >> anySingle) <|> noneOf ",\n")
-- Значение в кавычках
quotedValue :: Parser String
quotedValue = do
    _ <- char '"'
    res <- many ((char '\\' >> anySingle) <|> noneOf "\"")
    _ <- char '"'
    return res

main :: IO ()
main = do
  case parse csvParser "" csv of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right xs -> print xs
  print csvRes