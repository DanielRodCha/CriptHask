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

bindir     = "/home/jalonso/.cabal/bin"
libdir     = "/home/jalonso/.cabal/lib/x86_64-linux-ghc-7.10.2/toolshed-0.16.0.0-6rq7xtkouyb18M99AzDeAh"
datadir    = "/home/jalonso/.cabal/share/x86_64-linux-ghc-7.10.2/toolshed-0.16.0.0"
libexecdir = "/home/jalonso/.cabal/libexec"
sysconfdir = "/home/jalonso/.cabal/etc"

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
