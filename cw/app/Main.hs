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

----------------------------------

data Result = Result
  { teamA :: String
  , teamB :: String
  , resA :: Int
  , resB :: Int
  } deriving (Eq,Show,Read)

addResult :: [(String,Int)]
          -> Result
          -> [(String,Int)]
addResult state0 r = let
  (rA,rB) = case compare (resA r) (resB r) of
    LT -> (0,3)
    EQ -> (1,1)
    GT -> (3,0)
  tA = teamA r
  tB = teamB r
  add t r list = case lookup t list of
    Nothing -> (t,r) : list
    Just r0 -> (t,r0+r) : delete (t,r0) list
  in add tA rA $ add tB rB state0

sol3 :: [Result]
     -> [(String, Int)]
sol3 = foldl' addResult initState
  where initState = []

main :: IO ()
main = putStrLn "Hello, Haskell!"
