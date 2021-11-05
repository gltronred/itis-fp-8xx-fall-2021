{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment
import System.Directory
import Data.List
import Data.Char
import Control.Monad
import Control.Monad.State
import Control.Exception.Base
import Control.Monad.Except
import Data.Typeable

dirCount dir = do
	files <- ls dir
	return (length files)

dirSize dir = do
	files <- ls dir
	fmap sum $ sequence $ map liftIO $ map getFileSize files

allDirsByDepth :: FilePath -> Int -> StateT [(FilePath,(Int,Integer))] IO ()

allDirsByDepth dir 0 = do
  st <- get	
  count <- liftIO $ dirCount dir
  size <- liftIO $ dirSize dir
  put ((dir, (count, size)) : st)

allDirsByDepth dir depth = do
  st <- get	
  count <- liftIO $ dirCount dir
  size <- liftIO $ dirSize dir
  put ((dir, (count, size)) : st)
  dirs <- liftIO $ lsDirs dir
  forM_ dirs (\dir -> allDirsByDepth dir (depth - 1))

ls :: FilePath -> IO [FilePath]
ls dir = do 
  files <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case files of
    Left e -> do
      putStrLn $ "Error: " ++ show e
      return []
    Right files -> return $ mapToFullPath dir files

lsDirs dir = do 
  files <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case files of
    Left e -> do
      putStrLn $ "Error: " ++ show e
      return []
    Right files ->  filterM doesDirectoryExist (map (\fp -> dir ++ "/" ++ fp) files)

mapToFullPath dir xs = map (\x -> dir ++ "/" ++ x) xs

du' :: FilePath -> Int -> IO ()
du' path depth = do
  result <-  runStateT (allDirsByDepth path depth) []
  putStrLn "Counts"
  forM_ (map (\entry -> fst entry ++ " " ++ show ( fst $ snd entry)) (snd result)) putStrLn
  putStrLn "Sizes"
  forM_ (map (\entry -> fst entry ++ " " ++ show ( snd $ snd entry)) (snd result)) putStrLn


du :: FilePath -> Maybe Int -> IO ()
du path Nothing = du' path (maxBound :: Int)
du path (Just depth) = du' path depth

main = du "." (Just 2)
	