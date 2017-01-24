{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_haskell_project (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Wiesiek\\Desktop\\Haskell\\haskell-project\\.stack-work\\install\\93264ba9\\bin"
libdir     = "C:\\Users\\Wiesiek\\Desktop\\Haskell\\haskell-project\\.stack-work\\install\\93264ba9\\lib\\x86_64-windows-ghc-8.0.1\\haskell-project-0.1.0.0-40Qus2qNav6FY2bGilXhPo"
datadir    = "C:\\Users\\Wiesiek\\Desktop\\Haskell\\haskell-project\\.stack-work\\install\\93264ba9\\share\\x86_64-windows-ghc-8.0.1\\haskell-project-0.1.0.0"
libexecdir = "C:\\Users\\Wiesiek\\Desktop\\Haskell\\haskell-project\\.stack-work\\install\\93264ba9\\libexec"
sysconfdir = "C:\\Users\\Wiesiek\\Desktop\\Haskell\\haskell-project\\.stack-work\\install\\93264ba9\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
