{- Linux library copier and binary shimmer
 -
 - Copyright 2013-2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# Language LambdaCase #-}

module Utility.LinuxMkLibs (
	installLib,
	parseLdd,
	runLdd,
	glibcLibs,
	gconvLibs,
	inTop,
) where

import Utility.PartialPrelude
import Utility.Directory
import Utility.SystemDirectory
import Utility.Process
import Utility.Monad
import Utility.Path
import Utility.Path.AbsRel
import Utility.Split
import Utility.Env
import Utility.Exception
import Utility.OsPath
import Utility.RawFilePath

import Data.Maybe
import System.Posix.Files (isSymbolicLink)
import Data.Char
import Control.Monad.IfElse

{- Installs a library. If the library is a symlink to another file,
 - install the file it links to, and update the symlink to be relative. -}
installLib :: (OsPath -> OsPath -> IO ()) -> OsPath -> OsPath -> IO (Maybe OsPath)
installLib installfile top lib = ifM (doesFileExist lib)
	( do
		installfile top lib
		checksymlink lib
		return $ Just $ parentDir lib
	, return Nothing
	)
  where
	checksymlink f = whenM (isSymbolicLink <$> getSymbolicLinkStatus (fromOsPath (inTop top f))) $ do
		l <- readSymbolicLink (fromOsPath (inTop top f))
		let absl = absPathFrom (parentDir f) (toOsPath l)
		target <- relPathDirToFile (takeDirectory f) absl
		installfile top absl
		removeWhenExistsWith removeFile (inTop top f)
		createSymbolicLink (fromOsPath target) (fromOsPath (inTop top f))
		checksymlink absl

-- Note that f is not relative, so cannot use </>
inTop :: OsPath -> OsPath -> OsPath
inTop top f = top <> f

{- Parse ldd output, getting all the libraries that the input files
 - link to. Note that some of the libraries may not exist 
 - (eg, linux-vdso.so) -}
parseLdd :: String -> [OsPath]
parseLdd = map toOsPath 
	. mapMaybe (getlib . dropWhile isSpace)
	. lines
  where
	getlib l = headMaybe . words =<< lastMaybe (split " => " l)
	
runLdd :: [OsPath] -> IO [OsPath]
runLdd exes = concat <$> mapM go exes
  where
	go exe = tryNonAsync (readProcess "ldd" [fromOsPath exe]) >>= \case
		Right o -> return (parseLdd o)
		-- ldd for some reason segfaults when run in an arm64
		-- chroot on an amd64 host, on a binary produced by ghc.
		-- But asking ldd to trace loaded objects works.
		Left _e -> do
			environ <- getEnvironment
			let environ' =("LD_TRACE_LOADED_OBJECTS","1"):environ
			parseLdd <$> readProcessEnv (fromOsPath exe) [] (Just environ')

{- Get all glibc libs, and also libgcc_s
 -
 - XXX Debian specific. -}
glibcLibs :: IO [OsPath]
glibcLibs = do
	ls <- lines <$> readProcess "sh"
		["-c", "dpkg -L libc6:$(dpkg --print-architecture) | egrep '\\.so' | grep -v /gconv/ | grep -v ld.so.conf | grep -v sotruss-lib"]
	ls2 <- lines <$> readProcess "sh"
		["-c", "(dpkg -L libgcc-s1:$(dpkg --print-architecture 2>/dev/null) || dpkg -L libgcc1:$(dpkg --print-architecture)) | egrep '\\.so'"]
	return (map toOsPath (ls++ls2))

{- Get gblibc's gconv libs, which are handled specially.. -}
gconvLibs :: IO [OsPath]
gconvLibs = map toOsPath . lines <$> readProcess "sh"
	["-c", "dpkg -L libc6:$(dpkg --print-architecture) | grep /gconv/"]

