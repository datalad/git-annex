{- Linux library copier and binary shimmer
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.LinuxMkLibs (
	installLib,
	parseLdd,
	glibcLibs,
	gconvLibs,
	inTop,
) where

import Utility.PartialPrelude
import Utility.Directory
import Utility.Process
import Utility.Monad
import Utility.Path
import Utility.Path.AbsRel
import Utility.Split
import Utility.FileSystemEncoding

import Data.Maybe
import System.FilePath
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
		return $ Just $ fromRawFilePath $ parentDir $ toRawFilePath lib
	, return Nothing
	)
  where
	checksymlink f = whenM (isSymbolicLink <$> getSymbolicLinkStatus (inTop top f)) $ do
		l <- readSymbolicLink (inTop top f)
		let absl = absPathFrom
			(parentDir (toRawFilePath f))
			(toRawFilePath l)
		target <- relPathDirToFile (toRawFilePath (takeDirectory f)) absl
		installfile top (fromRawFilePath absl)
		removeWhenExistsWith removeLink (top ++ f)
		createSymbolicLink (fromRawFilePath target) (inTop top f)
		checksymlink (fromRawFilePath absl)

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

{- Get all glibc libs, and also libgcc_s
 -
 - XXX Debian specific. -}
glibcLibs :: IO [FilePath]
glibcLibs = do
	ls <- lines <$> readProcess "sh"
		["-c", "dpkg -L libc6:$(dpkg --print-architecture) | egrep '\\.so' | grep -v /gconv/ | grep -v ld.so.conf | grep -v sotruss-lib"]
	ls2 <- lines <$> readProcess "sh"
		["-c", "(dpkg -L libgcc-s1:$(dpkg --print-architecture 2>/dev/null) || dpkg -L libgcc1:$(dpkg --print-architecture)) | egrep '\\.so'"]
	return (ls++ls2)

{- Get gblibc's gconv libs, which are handled specially.. -}
gconvLibs :: IO [FilePath]
gconvLibs = lines <$> readProcess "sh"
	["-c", "dpkg -L libc6:$(dpkg --print-architecture) | grep /gconv/"]

