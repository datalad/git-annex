{- Linux library copier and binary shimmer
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.LinuxMkLibs where

import Utility.PartialPrelude
import Utility.Directory
import Utility.Process
import Utility.Monad
import Utility.Path

import Data.Maybe
import System.Directory
import System.FilePath
import Data.List.Utils
import System.Posix.Files
import Data.Char
import Control.Monad.IfElse
import Control.Applicative
import Prelude

{- Installs a library. If the library is a symlink to another file,
 - install the file it links to, and update the symlink to be relative. -}
installLib :: (FilePath -> FilePath -> IO ()) -> FilePath -> FilePath -> IO (Maybe FilePath)
installLib installfile top lib = ifM (doesFileExist lib)
	( do
		installfile top lib
		checksymlink lib
		return $ Just $ parentDir lib
	, return Nothing
	)
  where
	checksymlink f = whenM (isSymbolicLink <$> getSymbolicLinkStatus (inTop top f)) $ do
		l <- readSymbolicLink (inTop top f)
		let absl = absPathFrom (parentDir f) l
		target <- relPathDirToFile (takeDirectory f) absl
		installfile top absl
		nukeFile (top ++ f)
		createSymbolicLink target (inTop top f)
		checksymlink absl

-- Note that f is not relative, so cannot use </>
inTop :: FilePath -> FilePath -> FilePath
inTop top f = top ++ f

{- Parse ldd output, getting all the libraries that the input files
 - link to. Note that some of the libraries may not exist 
 - (eg, linux-vdso.so) -}
parseLdd :: String -> [FilePath]
parseLdd = mapMaybe (getlib . dropWhile isSpace) . lines
  where
	getlib l = headMaybe . words =<< lastMaybe (split " => " l)

{- Get all glibc libs and other support files, including gconv files
 -
 - XXX Debian specific. -}
glibcLibs :: IO [FilePath]
glibcLibs = lines <$> readProcess "sh"
	["-c", "dpkg -L libc6:$(dpkg --print-architecture) libgcc1:$(dpkg --print-architecture) | egrep '\\.so|gconv'"]
