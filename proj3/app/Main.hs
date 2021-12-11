module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

type Parser = Parsec Void String

csvParser :: Parser [[String]]
csvParser = sepEndBy parserLine (char '\n')

parserLine :: Parser [String]
parserLine = sepEndBy parserCell (char ',')

parserCell :: Parser String
parserCell = parserCellQuotas <|> parcerCommonCell

parserCellQuotas :: Parser String
parserCellQuotas = char '"' *> many ((char '\\' >> anySingle) <|> noneOf "\"") <* char '"'

parcerCommonCell :: Parser String
parcerCommonCell = many ((char '\\' >> anySingle) <|> noneOf ",\n" )

csv :: String
csv = "col1,col2,col3\nr2 c1,r2 c2,r2 c3\n\"r3,c1\",\"r3,c2\",\"r3,c3\"\n\"r4\\\",\\\"\\\\c1\",\"r4\\\",\\\"c2\",\"r4\\\",\\\"c3\""

csvRes :: [[String]]
csvRes =
  [ [ "col1", "col2", "col3" ]
  , [ "r2 c1", "r2 c2", "r2 c3" ]
  , [ "r3,c1", "r3,c2", "r3,c3" ]
  , [ "r4\",\"\\c1", "r4\",\"c2", "r4\",\"c3"]
  ]

main :: IO ()
main = parseTest csvParser csv