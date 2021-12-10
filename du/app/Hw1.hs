module Hw1 where

import System.Directory ( listDirectory, getFileSize, doesDirectoryExist )
import Control.Monad.Writer ( when )
import Control.Exception.Base ( SomeException, try )

data FileTree a = File a | Folder FilePath Int a [FileTree a] deriving (Show)

tryGetFileSize :: FilePath -> IO Integer
tryGetFileSize path = do
    sizeE <- try (getFileSize path) :: IO (Either SomeException Integer)
    case sizeE of
        Left exception -> 
            ( do
                putStrLn $ "Exception: " ++ show exception
                return 0
            )
        Right size -> 
            ( do
                return size
            )

tryListDirectory :: FilePath -> IO [FilePath]
tryListDirectory path = do
    pathsE <- try (listDirectory path) :: IO (Either SomeException [FilePath])
    case pathsE of
        Left exception -> 
            ( do
                putStrLn $ "Exception: " ++ show exception
                return []
            )
        Right paths -> 
            ( do
                return paths
            )

formFileTree :: Int -> FilePath -> IO (FileTree Integer)
formFileTree 0 path = do
    flag <- doesDirectoryExist path
    if flag
        then do
            return $ File 0
        else do    
            fileSize <- tryGetFileSize path
            return $ File fileSize

formFileTree depth path = do
    flag <- doesDirectoryExist path
    if flag
        then do
            paths <- tryListDirectory path
            files <- mapM (formFileTree (depth - 1) . ((path ++ "\\") ++)) paths
            return $ Folder path (length files) (foldl sumSize 0 files) files
        else do
            fileSize <- getFileSize path
            return $ File fileSize
    where
        sumSize :: Integer -> FileTree Integer -> Integer
        sumSize a (File b) = a + b
        sumSize a (Folder _ _ b _) = a + b

getFolders :: FileTree Integer -> IO [(FilePath, Int, Integer)]
getFolders (Folder path cnt size files) = do
    subFiles <- mapM getFolders files
    return $ (path, cnt, size) : concat subFiles
getFolders (File _) = do return []

helper :: FilePath -> Int -> IO ()
helper path depth = do
    fileTree <- formFileTree depth path
    folders <- getFolders fileTree
    putStrLn "Count:"
    mapM_ (\(path, cnt, _) -> putStrLn $ path ++ " " ++ show cnt) folders
    putStrLn "Size:"
    mapM_ (\(path, _, size) -> putStrLn $ path ++ " " ++ show size) folders

validateDepth :: Int -> IO Bool
validateDepth depth = if depth < 0
    then do
        putStrLn "Exception: Depth cannot be negative."
        return False
    else
        return True

du :: FilePath -> Maybe Int -> IO ()
du path Nothing = helper path(maxBound :: Int) 
du path (Just depth) = do
    isDepthValid <- validateDepth depth
    when isDepthValid $ helper path (depth + 1)