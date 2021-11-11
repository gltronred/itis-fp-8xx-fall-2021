module Main where

import Control.Exception.Base
import Control.Monad.State
import System.Directory (doesDirectoryExist, getFileSize, listDirectory)
import Data.Maybe (fromMaybe)

main = du ".." (Just 4)

du :: FilePath -> Maybe Int -> IO ()
du currPath depth = duFoldersAndFilesSizeExtractor currPath $ fromMaybe 0 depth

getFileSizeAsString :: FilePath -> IO String
getFileSizeAsString сurrPath = do
  size <- getFileSize сurrPath
  return $ сurrPath ++ " " ++ show size

getFilesCount :: FilePath -> IO String
getFilesCount сurrPath = do
  dirFilesCount <- length <$> listDirectory сurrPath
  pure $ сurrPath ++ " " ++ show dirFilesCount


duFoldersAndFilesSizeExtractor :: FilePath -> Int -> IO ()
duFoldersAndFilesSizeExtractor currPath depth = do
  result <- runStateT (getDirectoriesFromCurrent currPath depth) []
  size <- forM (snd result) getFileSizeAsString
  count <- forM (snd result) getFilesCount
  putStrLn "Count:"
  forM_ count putStrLn
  putStrLn "Size:"
  forM_ size putStrLn


getDirectoriesFromCurrent :: FilePath -> Int -> StateT [FilePath] IO ()
getDirectoriesFromCurrent currPath 0 = do modify (currPath :)
getDirectoriesFromCurrent currPath depth = do
  modify (currPath :)
  subDirectories <- liftIO $ getSubDirectories currPath
  forM_ subDirectories (\direcotory -> getDirectoriesFromCurrent direcotory (pred depth))


getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories currentPath = do
  result <- try (listDirectory currentPath) :: IO (Either SomeException [FilePath])
  case result of
    Left some_ex -> do
          putStrLn $ "Exception on listDirectory operation: " ++ show some_ex
          pure []
    Right files -> filterM doesDirectoryExist (map (\file -> currentPath ++ "/" ++ file) files)
