module Paths_vindinium (
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

bindir     = "/media/octopuscabbage/96ba6217-d45d-44dc-871e-70462d549341/docsandsettings/code/vindinium/GoodChuckWoodChuck/.cabal-sandbox/bin"
libdir     = "/media/octopuscabbage/96ba6217-d45d-44dc-871e-70462d549341/docsandsettings/code/vindinium/GoodChuckWoodChuck/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/vindinium-0.1.0.0"
datadir    = "/media/octopuscabbage/96ba6217-d45d-44dc-871e-70462d549341/docsandsettings/code/vindinium/GoodChuckWoodChuck/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/vindinium-0.1.0.0"
libexecdir = "/media/octopuscabbage/96ba6217-d45d-44dc-871e-70462d549341/docsandsettings/code/vindinium/GoodChuckWoodChuck/.cabal-sandbox/libexec"
sysconfdir = "/media/octopuscabbage/96ba6217-d45d-44dc-871e-70462d549341/docsandsettings/code/vindinium/GoodChuckWoodChuck/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "vindinium_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "vindinium_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "vindinium_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vindinium_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vindinium_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
