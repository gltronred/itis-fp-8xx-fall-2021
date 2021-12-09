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
csvParser = sepEndBy singleLineParser (char '\n')

singleLineParser :: Parser [String]
singleLineParser = sepEndBy singleCellParser (char ',')

singleCellParser :: Parser String
singleCellParser = cellInQuotasParser <|> usuallCellParser

-- (char '\\' >> anySingle) - allow us to parse escapedToken
usuallCellParser :: Parser String
usuallCellParser = many ( noneOf ",\n" <|> (char '\\' >> anySingle))

cellInQuotasParser :: Parser String
cellInQuotasParser = char '"' *> many ((char '\\' >> anySingle) <|> noneOf "\"") <* char '"'

main :: IO ()
main = parseTest csvParser csv
