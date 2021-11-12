module Main where

import Data.Function (on)
import Data.List

in1 :: [(String,Int)]
in1 = [("abc",11),("def",12)
      ,("ghi",12),("jkl",10)]

avgPlace :: [(Int, (String, Int))]
         -> Double
avgPlace xs = let
  s = sum $ map fst xs
  n = length xs
  in fromIntegral s / fromIntegral n

toResult :: [(Int, (String, Int))]
         -> [(String, Double)]
toResult xs = map (\x -> (fst $ snd x, avg)) xs
  where avg = avgPlace xs

sol1 :: [(String, Int)]
     -> [(String, Double)]
sol1 = concatMap toResult .
       groupBy ((==) `on` snd.snd) .
       zip [1..] .
       sortBy (compare `on` negate . snd)

main :: IO ()
main = putStrLn "Hello, Haskell!"
