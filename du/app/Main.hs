{-# LANGUAGE FlexibleContexts #-}

module Main where

    import System.Directory (listDirectory, doesDirectoryExist, getFileSize)
    import Control.Monad
    import Control.Monad.Writer (MonadIO, Writer, WriterT, tell, execWriterT)
    import Control.Monad.State (runStateT, MonadState, StateT, modify, get)
    import Control.Monad.Trans (liftIO)
    import Control.Monad.Except (runExcept, runExceptT, Except, ExceptT, MonadError, throwError)

    onError :: (MonadError String m, MonadIO m)
            => Bool -> String -> m ()
    onError b msg = if b
                    then liftIO $ print msg
                    else pure ()


    type Env = [(String, Integer, Integer)]

    main :: IO ()
    main = do

       ans <- run "." (2) []

       case ans of
            Left s -> print s
            Right (_,env) -> do

                putStrLn "Count:"

                forM_ env $ \(path, _, count) -> do
                      putStrLn $ path ++ " " ++ show count

                putStrLn "Size:"

                forM_ env $ \(path, size, _) -> do
                      putStrLn $ path ++ " " ++ show size

    run :: FilePath -> Int -> Env -> IO (Either String (Integer, Env))
    run path depth env = do
            runExceptT $ runStateT ev env
      where ev :: StateT Env (ExceptT String IO) Integer
            ev = dfs path $ depth


    dfs :: ( MonadError String m
           , MonadState Env m, MonadIO m )
          => FilePath -> Int -> m Integer
    dfs path depth = do
        isDir <- liftIO $ doesDirectoryExist path

        size <- case isDir of
                    True -> do
                        paths <- liftIO $ listDirectory path

                        pathsSizes <- forM paths $ \p -> do
                                                let fullName = path ++ "/" ++ p
                                                pure $ if depth == 0
                                                    then liftIO $ getFileSize fullName
                                                    else dfs fullName (depth - 1)

                        size <- fmap sum (sequence pathsSizes)
                        let count = fromIntegral $ length paths
                        modify $ \st -> (path, size, count) : st

                        pure $ size
                    False -> do
                        size <- liftIO $ getFileSize path
                        pure $ size

        pure size