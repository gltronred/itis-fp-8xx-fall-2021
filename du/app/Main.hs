module Main where

import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get, put),
    StateT,
    execStateT,
    filterM,
  )
import System.Directory
  ( doesDirectoryExist,
    getCurrentDirectory,
    getFileSize,
    listDirectory,
  )

getOnlyDirectories :: FilePath -> IO [FilePath]
getOnlyDirectories path = do
  dirContent <- listDirectory path
  filterM doesDirectoryExist (preparePath path dirContent)

preparePath :: String -> [String] -> [FilePath]
preparePath path = map (\fp -> path ++ "/" ++ fp)

getDirectoryWithSize :: FilePath -> IO String
getDirectoryWithSize path = do
  size <- getFileSize path
  return $ path ++ " " ++ show size

getDirectoryWithElementsCount :: FilePath -> IO String
getDirectoryWithElementsCount path = do
  v <- listDirectory path
  return $ path ++ " " ++ show (length v)

placeDirsToState :: FilePath -> Int -> StateT [FilePath] IO ()
placeDirsToState path 0 = do
  prev <- get
  put (path : prev)
placeDirsToState path depth = do
  prev <- get
  put (path : prev)
  dirs <- liftIO $ getOnlyDirectories path
  mapM_ (\dir -> placeDirsToState dir (depth - 1)) dirs

duHelper :: FilePath -> Int -> IO ()
duHelper path depth = do
  folders <- execStateT (placeDirsToState path depth) []
  counts <- mapM getDirectoryWithElementsCount folders
  sizes <- mapM getDirectoryWithSize folders
  putStrLn "Count:"
  mapM_ putStrLn (reverse counts)
  putStrLn "Size:"
  mapM_ putStrLn (reverse sizes)

du :: FilePath -> Maybe Int -> IO ()
du path Nothing = duHelper path (maxBound :: Int)
du path (Just depth) = duHelper path depth

main = do
  dir <- getCurrentDirectory
  du dir Nothing