module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

type Parser = Parsec Void String

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
csvParser = sepBy rowPraser (single '\n')

rowPraser :: Parser [String]
rowPraser = sepBy (quotedValueParser <|> (itemParser ",\n")) (single ',')

quotedCellParser :: Parser String
quotedCellParser = do
    _ <- char '"'
    result <- getValue "\""
    _ <- char '"'
    return result

valueParser :: Parser String
valueParser = many $ (char '\\' >> anySingle) <|> noneOf ",\n"

main :: IO ()
main = do
  case parse csvParser "" csv of
    Left error  -> putStr $ errorBundlePretty error
    Right result -> print result
  print csvRes 
