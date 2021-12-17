{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Exception.Base
import System.Environment
import Control.Monad.State
import System.Directory


getDirectories path = do
  r <- (try $ listDirectory  path ) :: IO (Either SomeException [FilePath] )
  case r of
    Left e ->
       do
          putStrLn $ "Exception: " ++ show e
          return []
    Right files -> filterM doesDirectoryExist (map (\f -> path ++ "/" ++ f) files)

getDirectoriesWithDepth :: FilePath -> Int -> StateT [FilePath] IO ()
getDirectoriesWithDepth path 0 = do
  dList <- get
  put (path : dList)

getDirectoriesWithDepth path  depth = do
  dList <- get
  put (path : dList)
  dirs <- liftIO $ getDirectories path
  forM_ dirs (\dir -> getDirectoriesWithDepth dir (depth -1))

getDirectoryFilesCount :: FilePath -> IO String
getDirectoryFilesCount path = do
  c <- fmap length $ listDirectory path
  return $ path ++ "\t" ++ show (c)

getDirectorySize :: FilePath -> IO String
getDirectorySize path = do
  s <- getFileSize path
  return $ path ++ "\t" ++ show (s)

du' :: FilePath -> Int -> IO ()
du' path depth = do
  res <- runStateT (getDirectoriesWithDepth path depth) []
  count <- forM (snd res) getDirectoryFilesCount
  size <- forM (snd res) getDirectorySize
  putStrLn "Count:"
  forM_ (reverse count) putStrLn
  putStrLn "Size:"
  forM_ (reverse size) putStrLn

du :: FilePath -> Maybe Int -> IO ()
du path Nothing = du' path (maxBound :: Int)
du path (Just depth) = du' path depth

main :: IO ()
main = do
  du "." (Just 0)
  putStrLn ("--------------------")
  du "." Nothing
