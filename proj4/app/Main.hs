module Main where
sumAndTabulate :: Int -> [Int] -> IO Int
sumAndTabulate cols list = sumTable cols cols list where
  sumTable _ _ [] = do
      putStr "\n"
      return 0
  sumTable current cols (num:list) = do
    if current == cols then putStr "\n"
    else putStr ""
    putStr $ "\t" ++ show num
    if current == 1 then do
      sum <- sumTable cols cols list
      return $ num + sum
    else do
      sum <- sumTable (current - 1) cols list
      return $ num + sum

main :: IO ()
main = do
  sum <- sumAndTabulate 5 [1..23]
  putStrLn $ show sum
