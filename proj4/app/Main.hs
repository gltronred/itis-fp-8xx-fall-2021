module Main where

sumAndTabulate cols list = printTable 0 cols list

printTable :: Int -> Int -> [Int] -> IO ()
printTable skipCount rowLength items
  | skipCount < (length items) = do
    printRow (take rowLength $ drop skipCount items)
    printTable (skipCount + rowLength) rowLength items
  | otherwise = do
    putStrLn (show $ sum items)

printRow :: [Int] -> IO ()
printRow [] = putStrLn ""
printRow (x:xs) = do
  putStr (show x ++ "\t")
  printRow xs