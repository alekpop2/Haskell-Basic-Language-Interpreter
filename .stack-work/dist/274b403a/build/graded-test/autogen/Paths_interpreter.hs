{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_interpreter (
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
version = Version [0,3,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\alekp\\OneDrive\\Desktop\\CS 440\\mp2-interpreter\\.stack-work\\install\\dc18c95b\\bin"
libdir     = "C:\\Users\\alekp\\OneDrive\\Desktop\\CS 440\\mp2-interpreter\\.stack-work\\install\\dc18c95b\\lib\\x86_64-windows-ghc-8.10.4\\interpreter-0.3.0.0-A031EuFFv2k9D2PG4oJsVp-graded-test"
dynlibdir  = "C:\\Users\\alekp\\OneDrive\\Desktop\\CS 440\\mp2-interpreter\\.stack-work\\install\\dc18c95b\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\Users\\alekp\\OneDrive\\Desktop\\CS 440\\mp2-interpreter\\.stack-work\\install\\dc18c95b\\share\\x86_64-windows-ghc-8.10.4\\interpreter-0.3.0.0"
libexecdir = "C:\\Users\\alekp\\OneDrive\\Desktop\\CS 440\\mp2-interpreter\\.stack-work\\install\\dc18c95b\\libexec\\x86_64-windows-ghc-8.10.4\\interpreter-0.3.0.0"
sysconfdir = "C:\\Users\\alekp\\OneDrive\\Desktop\\CS 440\\mp2-interpreter\\.stack-work\\install\\dc18c95b\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "interpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "interpreter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "interpreter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "interpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "interpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "interpreter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
