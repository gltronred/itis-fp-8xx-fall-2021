module Main where

import Control.Exception.Base
import Control.Monad.State
import System.Directory

main = du "." (Just 2)

du :: FilePath -> Maybe Int -> IO ()
du path Nothing = duHelper path (maxBound :: Int)
du path (Just depth) = duHelper path depth

duHelper :: FilePath -> Int -> IO ()
duHelper path depth = do
  folders <- runStateT (getDirs path depth) []
  sizes <- forM (snd folders) getFileSizeStr
  forM_ (reverse sizes) putStrLn
  counts <- forM (snd folders) getObjectCount
  forM_ (reverse counts) putStrLn


getDirs :: FilePath -> Int -> StateT [FilePath] IO ()
getDirs path 0 = do
  modify (path :)
getDirs path depth = do
  modify (path :)
  dirs <- liftIO $ getDirsInFolder path
  forM_ dirs (\dir -> getDirs dir (depth - 1))


getDirsInFolder :: FilePath -> IO [FilePath]
getDirsInFolder currentPath = do
  result <- try (listDirectory currentPath) :: IO (Either SomeException [FilePath])
  case result of
  	Right files -> filterM doesDirectoryExist (map (\fp -> currentPath ++ "/" ++ fp) files)
    Left ex ->
      ( do
          putStrLn $ "Exception occured: " ++ show ex
          return []
      )

getFileSizeStr :: FilePath -> IO String
getFileSizeStr path = do
  size <- getFileSize path
  return $ path ++ "\t" ++ show size

getObjectCount :: FilePath -> IO String
getObjectCount path = do
  result <- try (listDirectory path) :: IO (Either SomeException [FilePath])
  case result of
  	Right files -> return $ path ++ "\t" ++ show (length files)
    Left ex ->
      ( do
          putStrLn $ "Exception occured: " ++ show ex
          return $ path ++ "\t"
      )
