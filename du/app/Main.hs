module Main where

import System.Environment ( getArgs )
import Text.Read ( readMaybe )
import Data.Maybe ( isJust )
import Control.Monad.State
  ( 
    MonadIO (liftIO),
    MonadState (get, put),
    StateT,
    execStateT,
    filterM,
  )
import System.Directory
  ( 
    doesDirectoryExist,
    getCurrentDirectory,
    getFileSize,
    listDirectory,
  )

-- Получить список директорий по указанному пути
getDirectoryList :: FilePath -> IO [FilePath]
getDirectoryList path = do
  directoryList <- listDirectory path
  filterM doesDirectoryExist (getSubdirectoryPath path directoryList)

-- Получить путь вложенной директории
getSubdirectoryPath :: String -> [String] -> [FilePath]
getSubdirectoryPath path = map (\p -> path ++ "/" ++ p)

-- Получить список директорий с кол-вом файлом
getDirectoriesWithCounts :: FilePath -> IO String
getDirectoriesWithCounts path = do
  list <- listDirectory path
  return $ path ++ " " ++ show (length list)

-- Получить список директорий с общими размерами
getDirectoriesWithSizes :: FilePath -> IO String
getDirectoriesWithSizes path = do
  size <- getFileSize path
  return $ path ++ " " ++ show size

-- Получить дерево директорий с учетом глубины поиска через рекурсию
getDirectoryTree :: FilePath -> Int -> StateT [FilePath] IO ()
-- База рекурсии, т.е. достигнута заданная глубина
getDirectoryTree path 0 = do
  previous <- get
  put (path : previous)
-- Шаг рекурсии
getDirectoryTree path depth = do
  previous <- get
  put (path : previous)
  directoryList <- liftIO $ getDirectoryList path
  mapM_ (\directory -> getDirectoryTree directory (depth - 1)) directoryList

du' :: FilePath -> Int -> IO ()
du' path depth = do
  directories <- execStateT (getDirectoryTree path depth) []
  directoriesWithCounts <- mapM getDirectoriesWithCounts directories
  directoriesWithSizes <- mapM getDirectoriesWithSizes directories
  putStrLn "Count:"
  mapM_ putStrLn (reverse directoriesWithCounts)
  putStrLn "Size:"
  mapM_ putStrLn (reverse directoriesWithCounts)

du :: FilePath -> Maybe Int -> IO ()
du path Nothing = du' path (maxBound :: Int)
du path (Just depth) = du' path depth

main :: IO ()
main = do
    args <- getArgs
    let argsLength = length args
    if argsLength == 1 then
        du (head args) Nothing
    else if argsLength == 2 then do
        let depthMaybe = readMaybe (args !! 1) :: Maybe Int
        if isJust depthMaybe then
            du (head args) depthMaybe
        else
            putStrLn "Error: Input correct *Depth of search* value"
    else
        putStrLn "Error: Input correct arguments: *Directory* *Depth of search*"