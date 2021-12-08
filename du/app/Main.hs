module Main where

import Control.Monad.State
import Control.Exception 
import System.Directory

safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory parentPath = do
  res <- try (listDirectory parentPath) :: IO (Either SomeException [FilePath])
  case res of
    Left e -> ( 
      do
        putStrLn $ "Exception: " ++ show e
        return []
      )
    Right dirs -> return dirs

getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories parentPath = do
  subEntries <- safeListDirectory parentPath
  filterM doesDirectoryExist (map (\subPath -> parentPath ++ "/" ++ subPath) subEntries)

getDirectoryToSize :: FilePath -> IO (String, Integer)
getDirectoryToSize path = do
  size <- getFileSize path
  return $ (path, size)

getDirectoryToEntriesCount :: FilePath -> IO (String, Int)
getDirectoryToEntriesCount parentPath = do
  subEntries <- safeListDirectory parentPath
  return $ (parentPath, length subEntries)

getAllDirs :: FilePath -> Int -> StateT [FilePath] IO ()
getAllDirs path 0 = modify (path :)
getAllDirs path depth = do
  modify (path :)
  subDirs <- liftIO $ getSubDirectories path
  mapM_ (\subDir -> getAllDirs subDir $ pred depth) subDirs

du' :: FilePath -> Int -> IO ()
du' path depth = do
  dirs <- execStateT (getAllDirs path depth) []
  dirsToCounts <- mapM getDirectoryToEntriesCount dirs
  dirsToSizes <- mapM getDirectoryToSize dirs
  putStrLn "Count: "
  mapM_ (\(dir, count) -> putStrLn $ dir ++ " " ++ show count) dirsToCounts
  putStrLn "Size: "
  mapM_ (\(dir, size) -> putStrLn $ dir ++ " " ++ show size) dirsToSizes

du :: FilePath -> Maybe Int -> IO ()
du path Nothing = du' path (maxBound :: Int)
du path (Just depth) = du' path depth

main = du "." Nothing
