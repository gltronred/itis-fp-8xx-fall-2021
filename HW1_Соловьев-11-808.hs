module Homework1 (calc) where

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, liftM, filterM)

import Control.Exception (try, SomeException)
import System.IO (withFile, IOMode (ReadMode), hFileSize)

calc path depth = do
    result <- calcSize path depth
    return $ foldr (\x y -> (fst x + fst y, snd y + 1)) (0, 0) $ map (\x -> (snd x, 0)) result

calcSize :: FilePath -> Integer -> IO [(FilePath, Integer)]
calcSize path depth = do
  contents <- getDirCon path
  result <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            isFile <- doesFileExist newName
            if isDir && depth > 0
              then calcSize newName (depth - 1)
              else do 
                  if isFile then do
                    size <- filesize newName
                    return [(newName, size)]
                  else return []
  return $ concat result

getDirCon :: FilePath -> IO [String]
getDirCon path = do 
    result <- try (getDirectoryContents path) :: IO (Either SomeException [String])
    case result of
        Left ex  -> ( do 
                putStrLn $ "Exception: " ++ show ex
                return [])
        Right contents -> ( do 
                filteredResult <- return $ filter notDots contents
                return filteredResult)
    
notDots p = p /= "." && p /= ".."
  
filesize :: FilePath -> IO Integer
filesize path = do
    size <- try (withFile path ReadMode hFileSize) :: IO (Either SomeException Integer)
    case size of
        Left ex  -> ( do 
                putStrLn $ "Exception: " ++ show ex
                return 0)
        Right contents -> ( do 
                return contents)