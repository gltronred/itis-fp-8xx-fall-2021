module Main where
import Control.Monad.State
import Control.Exception 
import System.Directory

main = do
  dir <- getCurrentDirectory
  du dir Nothing

du :: FilePath -> Maybe Int -> IO ()
du path Nothing = duHelper path (maxBound :: Int)
du path (Just depth) = duHelper path depth

duHelper :: FilePath -> Int -> IO ()
duHelper path depth = do
  dirs <- execStateT (getAllDirs path depth) []
  dirCounts <- mapM getDirElementsCount dirs
  dirSizes <- mapM getDirSize dirs
  putStrLn "Count: "
  mapM_ (\(dir, count) -> putStrLn $ dir ++ " " ++ show count) dirCounts
  putStrLn "Size: "
  mapM_ (\(dir, size) -> putStrLn $ dir ++ " " ++ show size) dirSizes

getAllDirs :: FilePath -> Int -> StateT [FilePath] IO ()
getAllDirs path 0 = do
  prev <- get
  put (path : prev)
getAllDirs path depth = do
  modify (path :)
  subDirs <- liftIO $ getSubDirectories path
  mapM_ (\subDir -> getAllDirs subDir $ pred depth) subDirs

getDirElementsCount :: FilePath -> IO (String, Int)
getDirElementsCount parentPath = do
  subEntries <- safeListDirectory parentPath
  return $ (parentPath, length subEntries)

getDirSize :: FilePath -> IO (String, Integer)
getDirSize path = do
  size <- getFileSize path
  return $ (path, size)

getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories parentPath = do
  subEntries <- safeListDirectory parentPath
  filterM doesDirectoryExist (map (\subPath -> parentPath ++ "/" ++ subPath) subEntries)

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
