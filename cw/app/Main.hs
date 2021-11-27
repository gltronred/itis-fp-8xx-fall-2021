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

data Op
  = Plus -- сложение
  | Minus -- вычитание
  | Times -- умножение
  | Equals -- "равно"
  deriving (Eq,Show,Read)
data Button
  = Digit Int -- цифры от 0 до 9
  | Negate -- поменять знак
  | Operation Op -- операция
  | Clear -- очистить текущий результат
  | MemPlus -- добавить текущий результат в память
  | MemRestore -- заменить текущий результат числом из памяти
  | MemClear -- очистить
  deriving (Eq,Show,Read)

data State = State
  { prev :: Integer
  , oper :: Maybe Op
  , screen :: Integer
  , memory :: Integer
  } deriving (Eq,Show,Read)

apply :: Op -> Integer -> Integer -> Integer
apply o x y = case o of
  Plus -> x+y
  Minus -> x-y
  Times -> x*y
  Equals -> y

processButton :: State
              -> Button
              -> State
processButton State{ prev=p, oper=mo, screen=s, memory=m } b = case b of
  Digit d -> State p mo (10*s+fromIntegral d) m
  Negate -> State p mo (-s) m
  Clear -> State 0 Nothing 0 m
  Operation Equals -> case mo of
    Nothing -> State s Nothing 0 m
    Just op -> State (apply op p s) Nothing 0 m
  Operation op -> case mo of
    Nothing -> State s (Just op) 0 m
    Just o -> State (apply o p s) (Just op) 0 m
  MemPlus -> State p mo s (s + m)
  MemRestore -> State p mo m m
  MemClear -> State p mo s 0

initial :: State
initial = State 0 Nothing 0 0

sol2 :: [Button] -> Integer
sol2 = prev . foldl' processButton initial

in2 :: [Button]
in2 = [ Digit 1, Digit 2
      , Operation Plus
      , Digit 3
      , MemPlus
      , Operation Minus
      , Digit 7
      , Operation Equals
      , MemRestore]

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

in3 = [ Result "A" "B" 3 0
      , Result "B" "C" 0 3
      , Result "C" "A" 1 1]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ sol1 in1
  print $ sol2 in2
  print $ sol3 in3
