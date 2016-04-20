{- Linux library copier and binary shimmer
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Main where

import System.Environment
import Data.Maybe
import System.FilePath
import System.Directory
import Control.Monad
import Data.List
import System.Posix.Files
import Control.Monad.IfElse
import Control.Applicative
import Prelude

import Utility.LinuxMkLibs
import Utility.Directory
import Utility.Process
import Utility.Monad
import Utility.Path
import Utility.FileMode
import Utility.CopyFile

main :: IO ()
main = getArgs >>= go
  where
	go [] = error "specify LINUXSTANDALONE_DIST"
	go (top:_) = mklibs top

mklibs :: FilePath -> IO ()
mklibs top = do
	fs <- dirContentsRecursive top
	mapM_ symToHardLink fs
	exes <- filterM checkExe fs
	libs <- parseLdd <$> readProcess "ldd" exes
	glibclibs <- glibcLibs
	let libs' = nub $ libs ++ glibclibs
	libdirs <- nub . catMaybes <$> mapM (installLib installFile top) libs'

	-- Various files used by runshell to set up env vars used by the
	-- linker shims.
	writeFile (top </> "libdirs") (unlines libdirs)
	writeFile (top </> "gconvdir")
		(parentDir $ Prelude.head $ filter ("/gconv/" `isInfixOf`) glibclibs)
	
	let linker = Prelude.head $ filter ("ld-linux" `isInfixOf`) libs'
	mapM_ (installLinkerShim top linker) exes

{- Installs a linker shim script around a binary.
 -
 - Note that each binary is put into its own separate directory,
 - to avoid eg git looking for binaries in its directory rather
 - than in PATH.
 -
 - The linker is symlinked to a file with the same basename as the binary,
 - since that looks better in ps than "ld-linux.so".
 -}
installLinkerShim :: FilePath -> FilePath -> FilePath -> IO ()
installLinkerShim top linker exe = do
	createDirectoryIfMissing True (top </> shimdir)
	createDirectoryIfMissing True (top </> exedir)
	renameFile exe exedest
	link <- relPathDirToFile (top </> exedir) (top ++ linker)
	unlessM (doesFileExist (top </> exelink)) $
		createSymbolicLink link (top </> exelink)
	writeFile exe $ unlines
		[ "#!/bin/sh"
		, "GIT_ANNEX_PROGRAMPATH=\"$0\""
		, "export GIT_ANNEX_PROGRAMPATH"
#if ! MIN_VERSION_GLASGOW_HASKELL(7,10,0,0)
		-- workaround for https://ghc.haskell.org/trac/ghc/ticket/7695
		, "LOCPATH=/dev/null"
		, "export LOCPATH"
#endif
		, "exec \"$GIT_ANNEX_DIR/" ++ exelink ++ "\" --library-path \"$GIT_ANNEX_LD_LIBRARY_PATH\" \"$GIT_ANNEX_DIR/shimmed/" ++ base ++ "/" ++ base ++ "\" \"$@\""
		]
	modifyFileMode exe $ addModes executeModes
  where
	base = takeFileName exe
	shimdir = "shimmed" </> base
	exedir = "exe"
	exedest = top </> shimdir </> base
	exelink = exedir </> base

{- Converting symlinks to hard links simplifies the binary shimming
 - process. -}
symToHardLink :: FilePath -> IO ()
symToHardLink f = whenM (isSymbolicLink <$> getSymbolicLinkStatus f) $ do
	l <- readSymbolicLink f
	let absl = absPathFrom (parentDir f) l
	nukeFile f
	createLink absl f

installFile :: FilePath -> FilePath -> IO ()
installFile top f = do
	createDirectoryIfMissing True destdir
	void $ copyFileExternal CopyTimeStamps f destdir
  where
	destdir = inTop top $ parentDir f

checkExe :: FilePath -> IO Bool
checkExe f
	| ".so" `isSuffixOf` f = return False
	| otherwise = ifM (isExecutable . fileMode <$> getFileStatus f)
		( checkFileExe <$> readProcess "file" [f]
		, return False
		)

{- Check that file(1) thinks it's a Linux ELF executable, or possibly
 - a shared library (a few executables like ssh appear as shared libraries). -}
checkFileExe :: String -> Bool
checkFileExe s = and
	[ "ELF" `isInfixOf` s
	, "executable" `isInfixOf` s || "shared object" `isInfixOf` s
	]
