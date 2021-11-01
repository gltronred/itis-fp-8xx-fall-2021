{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State
import System.Directory (doesDirectoryExist, getCurrentDirectory, getFileSize, listDirectory)

getDirectoriesInSpecifiedFolder :: FilePath -> IO [FilePath]
getDirectoriesInSpecifiedFolder currentPath = do
  allFiles <- listDirectory currentPath
  filterM doesDirectoryExist (map (\fp -> currentPath ++ "/" ++ fp) allFiles)

getFileSizeStr :: FilePath -> IO String
getFileSizeStr path = do
  size <- getFileSize path
  return $ path ++ "\t" ++ show size

getObjectCount :: FilePath -> IO String
getObjectCount path = do
  v <- listDirectory path
  return $ path ++ "\t" ++ show (length v)

getDirectoriesByDepth :: FilePath -> Int -> StateT [FilePath] IO Bool
getDirectoriesByDepth path 0 = do
  modify (path :)
  return True
getDirectoriesByDepth path depth = do
  modify (path :)
  dirs <- liftIO $ getDirectoriesInSpecifiedFolder path
  forM_ dirs (\dir -> getDirectoriesByDepth dir (depth -1))
  return True

duHelper :: FilePath -> Int -> IO ()
duHelper path depth = do
    folders <- runStateT (getDirectoriesByDepth path depth) []
    counts <- forM (snd folders) getObjectCount
    sizes <- forM (snd folders) getFileSizeStr
    putStrLn "Count:"
    forM_ counts putStrLn
    putStrLn "Size:"
    forM_ sizes putStrLn

du :: FilePath -> Maybe Int -> IO ()
du path Nothing = duHelper path (maxBound:: Int)
du path (Just depth) = duHelper path depth

main = du "." (Just 1)