{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_proj4 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/aydar/.cabal/bin"
libdir     = "/home/aydar/.cabal/lib/x86_64-linux-ghc-8.6.5/proj4-0.1.0.0-7dp8yhYmEEsFKAepFv1tt0"
dynlibdir  = "/home/aydar/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/aydar/.cabal/share/x86_64-linux-ghc-8.6.5/proj4-0.1.0.0"
libexecdir = "/home/aydar/.cabal/libexec/x86_64-linux-ghc-8.6.5/proj4-0.1.0.0"
sysconfdir = "/home/aydar/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "proj4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "proj4_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "proj4_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "proj4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "proj4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "proj4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
