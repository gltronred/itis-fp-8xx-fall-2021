module Main where

import Control.Exception.Base ( SomeException, try )
import Control.Monad.State
    ( filterM,
      forM_,
      forM,
      MonadIO(liftIO),
      MonadState(put, get),
      StateT(runStateT) )
import System.Directory
    ( doesDirectoryExist, getFileSize, listDirectory )
import Data.Maybe (fromMaybe)
import System.Directory.Internal.Prelude (fromMaybe)

ls :: FilePath -> IO [FilePath]
ls path = do
  listDirectory path

getDirectories :: FilePath -> IO [FilePath]
getDirectories path = do
  result <- (try $ listDirectory path) :: IO (Either SomeException [FilePath] )
  case result of
    Left e -> do
          putStrLn $ "Exception: " ++ show e
          pure []
    Right files -> filterM doesDirectoryExist (map (\file -> path ++ "/" ++ file) files)

getDirectoriesWithDepth :: FilePath -> Int -> StateT [FilePath] IO ()
getDirectoriesWithDepth path 0 = do
  dList <- get
  put (dList ++ [path])
getDirectoriesWithDepth path depth = do
  dList <- get
  put (dList ++ [path])
  dirs <- liftIO $ getDirectories path
  forM_ dirs (\dir -> getDirectoriesWithDepth dir (depth -1))

getDirectoryFilesCount :: FilePath -> IO String
getDirectoryFilesCount path = do
  dirFilesCount <- length <$> listDirectory path
  pure $ path ++ " " ++ show dirFilesCount

getDirectorySize :: FilePath -> IO String
getDirectorySize path = do
  dirSize <- getFileSize path
  pure $ path ++ " " ++ show dirSize

du' :: FilePath -> Int -> IO ()
du' path depth = do
  res <- runStateT (getDirectoriesWithDepth path depth) []
  count <- forM (snd res) getDirectoryFilesCount
  size <- forM (snd res) getDirectorySize
  putStrLn "Count:"
  forM_ count putStrLn
  putStrLn "Size:"
  forM_ size putStrLn

du :: FilePath -> Maybe Int -> IO ()
du path depth = du' path (fromMaybe (maxBound  :: Int) depth)

main = do
  du ".." Nothing 