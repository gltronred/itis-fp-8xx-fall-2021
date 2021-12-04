import Control.Monad.State
import System.Directory

main = du "." (Just 2)

du :: FilePath -> Maybe Int -> IO ()
du path Nothing = duHelper path (maxBound :: Int)
du path (Just depth) = duHelper path depth

duHelper p depth = do
  folders <- execStateT (dirsState p depth) []
  counts <- mapM getDirsWithCount folders
  sizes <- mapM getDirWithSize folders
  putStrLn "Count:"
  mapM_ putStrLn (reverse counts)
  putStrLn "Size:"
  mapM_ putStrLn (reverse sizes)

dirsState :: FilePath -> Int -> StateT [FilePath] IO ()
dirsState p 0 = do
  prev <- get
  put (p : prev)
dirsState p depth = do
  prev <- get
  put (p : prev)
  ds <- liftIO (getDirs p)
  mapM_ (\d -> dirsState d (depth - 1)) ds

getDirs p = do
  dirContent <- listDirectory p
  filterM doesDirectoryExist (map (\fp -> p ++ "/" ++ fp) dirContent)

getDirsWithCount p = do
  dirContent <- listDirectory p
  return (p ++ " " ++ show (length dirContent))

getDirWithSize p = do
  size <- getFileSize p
  return (p ++ " " ++ show size)
