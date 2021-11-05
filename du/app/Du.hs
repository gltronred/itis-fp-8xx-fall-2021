module Du where

import System.Directory
    ( doesDirectoryExist, doesFileExist, getFileSize, listDirectory )
import Control.Exception.Base ( SomeException, try )

type FileSize = Integer
type FilesCount = Int

data DirTree =
    NotValid
    | DirectoryNotInDepth
    | Directory FilePath FileSize FilesCount [DirTree]
    | File FilePath FileSize
        deriving (Show)

getFileInfo :: FilePath -> IO DirTree
getFileInfo path = do
    isFile <- doesFileExist path
    if not isFile then do
        putStrLn $ "Error: file does not exist - " ++ path
        return NotValid
    else do
        fileSize <- try(getFileSize path) :: IO (Either SomeException Integer)
        case fileSize of
            Left e -> do
                putStrLn $ "Error: " ++ show e
                return NotValid
            Right fileSize ->
                return $ File path fileSize

buildTree :: FilePath -> Maybe Integer -> IO DirTree
buildTree path (Just (-1)) = do
    isDirectory <- doesDirectoryExist path
    if isDirectory then
        return DirectoryNotInDepth
    else
        getFileInfo path
buildTree path depth = do
    isDirectory <- doesDirectoryExist path
    if isDirectory then do
        pathsEither <- try(listDirectory path) :: IO (Either SomeException [FilePath])
        case pathsEither of
            Left e -> do
                putStrLn $ "Error: " ++ show e
                return NotValid
            Right paths -> do
                dirContent <- mapM (\x -> buildTree (path ++ "/" ++ x) (nextDepth depth)) paths
                return $ Directory path (foldl sumSize 0 dirContent) (foldl sumCount 0 dirContent) dirContent
    else
        getFileInfo path
    where
        -- Задание было:
        ---- Выдаёт для каждой директории не глубже DEPTH количество файлов в ней.
        -- Не совсем понятно ожидаемое поведение, поэтому снизу реализованы все трактовки

        -- Количество файлов в директории, включая файлы во вложенных директориях:
        -- [Подходит под пример из README.md]
        -- sumCount :: FilesCount -> DirTree -> FilesCount
        -- sumCount cur (File _ _) = cur + 1
        -- sumCount cur (Directory _ _ count _) = cur + count
        -- sumCount cur DirectoryNotInDepth = cur
        -- sumCount cur NotValid = cur

        -- Количество файлов в директории, не включая файлы во вложенных директориях
        -- [Не подходит под пример из README.md, зря делал наверное]
        -- sumCount :: FilesCount -> DirTree -> FilesCount
        -- sumCount cur (File _ _) = cur + 1
        -- sumCount cur Directory {} = cur
        -- sumCount cur DirectoryNotInDepth = cur
        -- sumCount cur NotValid = cur

        -- Количество файлов и директорий в директории, включая файлы и директории во вложенных директориях
        -- [Не подходит под пример из README.md, зря делал наверное]
        -- sumCount :: FilesCount -> DirTree -> FilesCount
        -- sumCount cur (File _ _) = cur + 1
        -- sumCount cur (Directory _ _ count _) = cur + count + 1
        -- sumCount cur DirectoryNotInDepth = cur + 1
        -- sumCount cur NotValid = cur


        -- Количество файлов и директорий в директории, не включая файлы и директории во вложенных директориях
        -- [Подходит под пример из README.md, скорее всего то, что имелось ввиду]
        sumCount :: FilesCount -> DirTree -> FilesCount
        sumCount cur (File _ _) = cur + 1
        sumCount cur Directory {} = cur + 1
        sumCount cur DirectoryNotInDepth = cur + 1
        sumCount cur NotValid = cur

        sumSize :: FileSize -> DirTree -> FileSize
        sumSize cur (File _ fSize) = cur + fSize
        sumSize cur (Directory _ dSize _ _) = cur + dSize
        sumSize cur DirectoryNotInDepth = cur
        sumSize cur NotValid = cur

        nextDepth :: Maybe Integer -> Maybe Integer
        nextDepth (Just depth) = Just (depth - 1)
        nextDepth Nothing = Nothing

writeCount :: DirTree -> IO()
writeCount dirTree = do
    putStrLn "Count:"
    writeCountInner dirTree
    where
        writeCountInner :: DirTree -> IO ()
        writeCountInner (File _ _) = putStr ""
        writeCountInner NotValid = putStr ""
        writeCountInner DirectoryNotInDepth = putStr ""
        writeCountInner (Directory path _ count dirContent) = do
            putStrLn $ path ++ " " ++ show count
            mapM_ writeCountInner dirContent

writeSize :: DirTree -> IO()
writeSize dirTree = do
    putStrLn "Size:"
    writeSizeInner dirTree
    where
        writeSizeInner :: DirTree -> IO ()
        writeSizeInner (File _ _) = putStr ""
        writeSizeInner NotValid = putStr ""
        writeSizeInner DirectoryNotInDepth = putStr ""
        writeSizeInner (Directory path size _ dirContent) = do
            putStrLn $ path ++ " " ++ show size
            mapM_ writeSizeInner dirContent

checkDepthInput :: (Ord a, Num a) => a -> Bool
checkDepthInput depth = depth >= 0

duInner :: FilePath -> Maybe Integer -> IO ()
duInner path depth = do
            dirTree <- buildTree path depth
            writeCount dirTree
            writeSize dirTree

du :: FilePath -> Maybe Integer -> IO ()
du path Nothing = duInner path Nothing
du path (Just depth) = do
    let depthValid = checkDepthInput depth
    if depthValid then
        duInner path (Just depth)
    else
        putStrLn "Error: DEPTH cannot be negative"
        