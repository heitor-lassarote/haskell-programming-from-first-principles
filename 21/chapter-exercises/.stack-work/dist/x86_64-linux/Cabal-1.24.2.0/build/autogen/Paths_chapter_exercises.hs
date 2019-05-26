{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_chapter_exercises (
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

bindir     = "/mnt/c/Users/Heitor/Documents/Computing/Haskell/haskell-programming-from-first-principles/21/chapter-exercises/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/bin"
libdir     = "/mnt/c/Users/Heitor/Documents/Computing/Haskell/haskell-programming-from-first-principles/21/chapter-exercises/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/lib/x86_64-linux-ghc-8.0.2/chapter-exercises-0.1.0.0-MyTkAmxHPpEzugi6gUoOh"
dynlibdir  = "/mnt/c/Users/Heitor/Documents/Computing/Haskell/haskell-programming-from-first-principles/21/chapter-exercises/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/mnt/c/Users/Heitor/Documents/Computing/Haskell/haskell-programming-from-first-principles/21/chapter-exercises/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/share/x86_64-linux-ghc-8.0.2/chapter-exercises-0.1.0.0"
libexecdir = "/mnt/c/Users/Heitor/Documents/Computing/Haskell/haskell-programming-from-first-principles/21/chapter-exercises/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/libexec"
sysconfdir = "/mnt/c/Users/Heitor/Documents/Computing/Haskell/haskell-programming-from-first-principles/21/chapter-exercises/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chapter_exercises_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chapter_exercises_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "chapter_exercises_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "chapter_exercises_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chapter_exercises_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chapter_exercises_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
