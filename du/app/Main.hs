{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Exception.Base
import Control.Monad.State
import System.Directory

getDirectoriesInSpecifiedFolder :: FilePath -> IO [FilePath]
getDirectoriesInSpecifiedFolder currentPath = do
  allFilesResult <- try (listDirectory currentPath) :: IO (Either SomeException [FilePath])
  case allFilesResult of
    Left se ->
      ( do
          putStrLn $ "Exception: " ++ show se
          return []
      )
    Right files -> filterM doesDirectoryExist (map (\fp -> currentPath ++ "/" ++ fp) files)

getFileSizeStr :: FilePath -> IO String
getFileSizeStr path = do
  size <- getFileSize path
  return $ path ++ "\t" ++ show size

getObjectCount :: FilePath -> IO String
getObjectCount path = do
  filesResult <- try (listDirectory path) :: IO (Either SomeException [FilePath])
  case filesResult of
    Left se ->
      ( do
          putStrLn $ "Exception: " ++ show se
          return $ path ++ "\t" ++ "(Unable to get elements count)"
      )
    Right files -> return $ path ++ "\t" ++ show (length files)

getDirectoriesByDepth :: FilePath -> Int -> StateT [FilePath] IO Bool
getDirectoriesByDepth path 0 = do
  modify (path :)
  return True
getDirectoriesByDepth path depth = do
  modify (path :)
  dirs <- liftIO $ getDirectoriesInSpecifiedFolder path
  forM_ dirs (\dir -> getDirectoriesByDepth dir (depth - 1))
  return True

duHelper :: FilePath -> Int -> IO ()
duHelper path depth = do
  folders <- runStateT (getDirectoriesByDepth path depth) []
  counts <- forM (snd folders) getObjectCount
  sizes <- forM (snd folders) getFileSizeStr
  putStrLn "Count:"
  forM_ (reverse counts) putStrLn
  putStrLn "Size:"
  forM_ (reverse sizes) putStrLn

du :: FilePath -> Maybe Int -> IO ()
du path Nothing = duHelper path (maxBound :: Int)
du path (Just depth) = duHelper path depth

main = du "." (Just 2)