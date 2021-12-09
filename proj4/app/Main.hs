module Main where

import Streaming as S
import qualified Streaming.Prelude as S

sumAndTabulate :: Int -> [Int] -> IO Int
sumAndTabulate cols list = do
   sumAndTabulate' cols list 1
   putStrLn "\n"
   return $ sum list

sumAndTabulate' cols (x : xs) cur
 | null xs = printNum x
 | cols == cur = do
   printLnNum x
   sumAndTabulate' cols xs 1
 | otherwise = do
   printNum x
   sumAndTabulate' cols xs (cur + 1)

printNum x = putStr $ "\t" ++ show x

printLnNum x = putStrLn $ "\t" ++ show x

main :: IO ()
main = do
 sum <- sumAndTabulate 3 [1..10]
 putStrLn $ "sum: " ++ show sum
 