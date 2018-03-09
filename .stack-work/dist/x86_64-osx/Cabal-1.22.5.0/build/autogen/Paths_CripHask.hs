module Paths_CripHask (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/danrodcha/CriptHask/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/bin"
libdir     = "/Users/danrodcha/CriptHask/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/lib/x86_64-osx-ghc-7.10.3/CripHask-0.1.0.0-6c6QSyYgErH80S6dnRE85H"
datadir    = "/Users/danrodcha/CriptHask/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/share/x86_64-osx-ghc-7.10.3/CripHask-0.1.0.0"
libexecdir = "/Users/danrodcha/CriptHask/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/libexec"
sysconfdir = "/Users/danrodcha/CriptHask/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CripHask_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CripHask_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "CripHask_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CripHask_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CripHask_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
