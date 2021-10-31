{-# LANGUAGE FlexibleContexts #-}
module Main where
import System.Directory (listDirectory, doesDirectoryExist, getFileSize, getCurrentDirectory)
import Control.Monad.State


getDirectoriesInSpecifiedFolder :: FilePath -> IO [FilePath]
getDirectoriesInSpecifiedFolder currentPath = do
    allFiles <- listDirectory currentPath
    filterM doesDirectoryExist (map (\fp -> currentPath ++ "/" ++ fp) allFiles)

getDirectoriesByDepth :: FilePath -> Int -> StateT [FilePath] IO Bool
getDirectoriesByDepth path 0 = do
    modify (path :)
    return True
getDirectoriesByDepth path depth = do
    modify (path :)
    dirs <- liftIO $ getDirectoriesInSpecifiedFolder path
    forM_ dirs (\dir -> getDirectoriesByDepth dir (depth-1))
    return True


helper :: (Eq t, Num t) => FilePath -> [FilePath] -> t -> IO [FilePath]
helper directory accum 0 = do
    return accum
helper directory accum depth = do
    dirs <- getDirectoriesInSpecifiedFolder directory
    helper (directory ++ "/" ++ head dirs) (accum ++ map (\d -> directory ++ "/" ++ d) dirs) (depth -1)

ioInsideState :: StateT [FilePath] IO Bool
ioInsideState = do
    v <- get
    put ["."]
    dirs <- liftIO $ getDirectoriesInSpecifiedFolder "."
    put $ ["f", "f2"] ++ dirs
    return True

checkioInsideState = do
    runStateT ioInsideState []

getFileSizeString path = do
    size <- getFileSize path
    return $ path ++ "      " ++ show size
main = do
    path <- getCurrentDirectory
    v <- runStateT (getDirectoriesByDepth path 2) []
    sizes <- forM (snd v) getFileSizeString
    --forM_ (snd v) (\path ->print (path ++ getFileSizeString path))
    forM_ sizes print
    