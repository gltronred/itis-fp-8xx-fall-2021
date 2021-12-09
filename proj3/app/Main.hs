module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

type Parser = Parsec Void String

csvParser :: Parser [[String]]
csvParser = sepBy rowPraser (single '\n')

rowPraser :: Parser [String]
rowPraser = sepBy (quotedValueParser <|> (itemParser ",\n")) (single ',')

quotedValueParser :: Parser String
quotedValueParser = do
    _ <- char '"'
    value <- itemParser "\""
    _ <- char '"'
    return value
    
itemParser :: String -> Parser String
itemParser parseBy = many $ (char '\\' >> anySingle) <|> noneOf parseBy

main :: IO ()
main = do
  case parse csvParser "" csv of
    Left ex  -> putStr $ errorBundlePretty ex
    Right res -> print res
  print csvRes 
