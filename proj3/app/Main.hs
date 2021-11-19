module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

type Parser = Parsec Void String

csv :: String
csv = "col1,col2,col3\nr2 c1,r2 c2,r2 c3\n\"r3,c1\",\"r3,c2\",\"r3,c3\"\n\"r4\\\",\\\"\\\\c1\",\"r4\\\",\\\"c2\",\"r4\\\",\\\"c3\""

csvParser :: Parser [[String]]
csvParser = sepBy parseRow (single '\n')

parseRow :: Parser [String]
parseRow = sepBy (quotedCellParser <|> getValue ",\n") (single ',')

getValue :: String -> Parser String
getValue sep = many ((char '\\' >> anySingle) <|> noneOf sep)

quotedCellParser :: Parser String
quotedCellParser = do
    _ <- char '"'
    result <- getValue "\""
    _ <- char '"'
    return result

main :: IO ()
main = parseTest csvParser csv
