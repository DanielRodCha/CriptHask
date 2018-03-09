module Paths_toolshed (
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
version = Version [0,16,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/danrodcha/CriptHask/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/bin"
libdir     = "/Users/danrodcha/CriptHask/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/lib/x86_64-osx-ghc-7.10.3/toolshed-0.16.0.0-3VVSfwRhtQOBrJAbe7B7f3"
datadir    = "/Users/danrodcha/CriptHask/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/share/x86_64-osx-ghc-7.10.3/toolshed-0.16.0.0"
libexecdir = "/Users/danrodcha/CriptHask/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/libexec"
sysconfdir = "/Users/danrodcha/CriptHask/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "toolshed_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "toolshed_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "toolshed_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "toolshed_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "toolshed_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
