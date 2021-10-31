module Main where
import Control.Monad.Writer.Lazy (WriterT, forM, filterM, MonadTrans (lift))
import System.Directory (listDirectory, doesDirectoryExist, getFileSize, getCurrentDirectory)


getDirectories :: FilePath -> IO [FilePath]
getDirectories currentPath = do
    allFiles <- listDirectory currentPath
    filterM doesDirectoryExist allFiles

main = do
    path <- getCurrentDirectory 
    print path
    size <- getFileSize path
    print size