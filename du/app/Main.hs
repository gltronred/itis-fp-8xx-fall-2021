module Main where

import System.Environment
import System.Directory
import Data.List
import Data.Char
import Control.Monad
import Control.Exception.Base
import Control.Monad.Except


main :: IO ()
main = do
    args <- getArgs
    if length args == 1 
    then do
      putStrLn "Count:"
      count <- recCount (args !! 0) (-1)
      putStrLn "Size:"
      size <- recSize (args !! 0) (-1)
      putStrLn ""
    else do
      putStrLn "Count:"
      count <- recCount (args !! 0) (read $ args !! 1)
      putStrLn "Size:"
      size <- recSize (args !! 0) (read $ args !! 1)
      putStrLn ""


getFromDir :: Bool -> [FilePath] -> IO [FilePath]
getFromDir files xs | files == True  = filterM doesFileExist xs
                    | files == False = filterM doesDirectoryExist xs

getFilesDirsSizes :: [FilePath] -> IO ([FilePath],[FilePath],Integer)
getFilesDirsSizes xs = do
  fs <- getFromDir True xs
  ds <- getFromDir False xs
  sz <- mapM tryGetSize fs
  return $ (fs, ds, foldr (+) 0 sz)

tryGetSize :: FilePath -> IO Integer
tryGetSize x = do
  res <- try (getFileSize x) :: IO (Either SomeException Integer)
  case res of
    Left e -> do
      putStrLn $ "Exception: " ++ show e
      return 0
    Right num -> return num


recCount :: FilePath -> Int -> IO Int
recCount dir depth = do
  xs <- tryReadDir dir
  (_,ds,_) <- getFilesDirsSizes xs
  if depth /= 0 && length ds /= 0
  then do 
    tmp_count <- sumM $ map (\x -> recCount x (depth - 1)) ds
    putStrLn $ dir ++ " " ++ (show $ length xs + tmp_count)
    return $ length xs + tmp_count
  else do
    putStrLn $ dir ++ " " ++ (show $ length xs)
    return $ length xs



recSize :: FilePath -> Int -> IO Integer
recSize dir depth = do
  xs <- tryReadDir dir
  (fs,ds,size) <- getFilesDirsSizes xs 
  if depth /= 0 && length ds /= 0
  then do 
    tmp_size <- sumM $ map (\x -> recSize x (depth - 1)) ds
    putStrLn $ dir ++ " " ++ (show $ size + tmp_size)
    return $ size + tmp_size
  else do
    putStrLn $ dir ++ " " ++ (show size)
    return $ size


tryReadDir :: FilePath -> IO [FilePath]
tryReadDir dir = do 
  files <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case files of
    Left e -> do
      putStrLn $ "Exception: " ++ show e
      return []
    Right fs -> return $ addPrevDir dir fs


sumM :: Num a => [IO a] -> IO a
sumM xs = foldr (\m1 m2 -> do {a <- m1; b <- m2; return $ a + b})  (return 0) xs


addPrevDir :: FilePath -> [FilePath] -> [FilePath]
addPrevDir dir xs = map (\x -> dir ++ "/" ++ x) xs


