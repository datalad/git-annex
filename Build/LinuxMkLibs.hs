{- Linux library copier and binary shimmer
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Build.LinuxMkLibs (mklibs) where

import Data.Maybe
import System.FilePath
import Control.Monad
import Data.List
import System.Posix.Files
import Control.Monad.IfElse
import Control.Applicative
import qualified System.Info
import Prelude

import Utility.LinuxMkLibs
import Utility.Directory
import Utility.Process
import Utility.Monad
import Utility.Path
import Utility.Path.AbsRel
import Utility.FileMode
import Utility.CopyFile
import Utility.FileSystemEncoding

mklibs :: FilePath -> a -> IO Bool
mklibs top _installedbins = do
	fs <- dirContentsRecursive top
	exes <- filterM checkExe fs
	libs <- runLdd exes
	
	glibclibs <- glibcLibs
	let libs' = nub $ libs ++ glibclibs
	let (linkers, otherlibs) = partition ("ld-linux" `isInfixOf`) libs'
	libdirs <- nub . catMaybes <$> mapM (installLib installFile top) otherlibs
	libdirs' <- consolidateUsrLib top libdirs

	gconvlibs <- gconvLibs
	mapM_ (installLib installFile top) gconvlibs

	-- Various files used by runshell to set up env vars used by the
	-- linker shims.
	writeFile (top </> "libdirs") (unlines libdirs')
	writeFile (top </> "gconvdir") (fromRawFilePath $ parentDir $ toRawFilePath $ Prelude.head gconvlibs)
	
	mapM_ (installLib installFile top) linkers
	let linker = Prelude.head linkers
	mapM_ (installLinkerShim top linker) exes
	
	return (any hwcaplibdir libdirs)
  where
	-- hwcap lib dirs are things like foo/tls and foo/x86.
	-- Hard to know if a directory is, so this is a heuristic
	-- looking for things that are certainly not. If this heuristic
	-- fails, a minor optimisation will not happen, but there will be
	-- no bad results.
	hwcaplibdir d = not $ or
		[ "lib" == takeFileName d
		-- eg, "lib/x86_64-linux-gnu"
		, "-linux-" `isInfixOf` takeFileName d
		]

{- If there are two libdirs that are the same except one is in
 - usr/lib and the other is in lib/, move the contents of the usr/lib one
 - into the lib/ one. This reduces the number of directories the linker
 - needs to look in, and so reduces the number of failed stats
 - and improves startup time.
 -}
consolidateUsrLib :: FilePath -> [FilePath] -> IO [FilePath]
consolidateUsrLib top libdirs = go [] libdirs
  where
	go c [] = return c
	go c (x:rest) = case filter (\d -> ("/usr" ++ d) == x) libdirs of
		(d:_) -> do
			fs <- getDirectoryContents (inTop top x)
			forM_ fs $ \f -> do
				let src = inTop top (x </> f)
				let dst = inTop top (d </> f)
				unless (dirCruft f) $
					unlessM (doesDirectoryExist src) $
						renameFile src dst
			symlinkHwCapDirs top d
			go c rest
		_ -> go (x:c) rest

{- The linker looks for optimised versions of libraries depending on the
 - hardware capabilities. That causes a lot of extra work searching for
 - libraries, so to avoid it, make symlinks from the hwcap directories
 - to the libdir. This way, the linker will find a library the first place
 - it happens to look for it.
 -}
symlinkHwCapDirs :: FilePath -> FilePath -> IO ()
symlinkHwCapDirs top libdir = forM_ hwcapdirs $ \d ->
	unlessM (doesDirectoryExist (top ++ libdir </> d)) $ do
		createDirectoryIfMissing True (top ++ libdir </> takeDirectory d)
		link <- relPathDirToFile
			(toRawFilePath (top ++ takeDirectory (libdir </> d)))
			(toRawFilePath (top ++ libdir))
		let link' = case fromRawFilePath link of
			"" -> "."
			l -> l
		createSymbolicLink link' (top ++ libdir </> d)
  where
	hwcapdirs = case System.Info.arch of
		"x86_64" -> 
			-- See glibc's sysdeps/x86_64/dl-hwcaps-subdirs.c 
			-- for list of subarchitecture directories.
			[ "glibc-hwcaps/x86-64-v2"
			, "glibc-hwcaps/x86-64-v3"
			, "glibc-hwcaps/x86-64-v4"
			-- The linker later checks these, and will check
			-- them when none of the above subarchitectures
			-- are supported by the processor, so make them
			-- just in case.
			, "tls/x86_64"
			, "x86_64"
			]
		"i386" ->
			[ "tls/i686"
			, "tls/i586"
			, "i686"
			, "i586"
			]
		"arm" ->
			-- Probably not complete, only what I have
			-- observed.
			[ "tls/v7l"
			, "v7l"
			]
		_ -> []

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
	ifM (isSymbolicLink <$> getSymbolicLinkStatus exe)
		( do
			sl <- readSymbolicLink exe
			removeWhenExistsWith removeLink exe
			removeWhenExistsWith removeLink exedest
			-- Assume that for a symlink, the destination
			-- will also be shimmed.
			let sl' = ".." </> takeFileName sl </> takeFileName sl
			createSymbolicLink sl' exedest
		, renameFile exe exedest
		)
	link <- relPathDirToFile
		(toRawFilePath (top </> exedir))
		(toRawFilePath (top ++ linker))
	unlessM (doesFileExist (top </> exelink)) $
		createSymbolicLink (fromRawFilePath link) (top </> exelink)
	writeFile exe $ unlines
		[ "#!/bin/sh"
		, "exec \"$GIT_ANNEX_DIR/" ++ exelink ++ "\" --library-path \"$GIT_ANNEX_LD_LIBRARY_PATH\" \"$GIT_ANNEX_DIR/shimmed/" ++ base ++ "/" ++ base ++ "\" \"$@\""
		]
	modifyFileMode (toRawFilePath exe) $ addModes executeModes
  where
	base = takeFileName exe
	shimdir = "shimmed" </> base
	exedir = "exe"
	exedest = top </> shimdir </> base
	exelink = exedir </> base

installFile :: FilePath -> FilePath -> IO ()
installFile top f = do
	createDirectoryIfMissing True destdir
	void $ copyFileExternal CopyTimeStamps f destdir
  where
	destdir = inTop top $ fromRawFilePath $ parentDir $ toRawFilePath f

checkExe :: FilePath -> IO Bool
checkExe f
	| ".so" `isSuffixOf` f = return False
	| otherwise = ifM (isExecutable . fileMode <$> getFileStatus f)
		( checkFileExe <$> readProcess "file" ["-L", f]
		, return False
		)

{- Check that file(1) thinks it's a Linux ELF executable, or possibly
 - a shared library (a few executables like ssh appear as shared libraries). -}
checkFileExe :: String -> Bool
checkFileExe s = and
	[ "ELF" `isInfixOf` s
	, "executable" `isInfixOf` s || "shared object" `isInfixOf` s
	]
