{- directory traversal and manipulation
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs -w #-}

module Utility.Directory (
	module Utility.Directory,
	module System.Directory
) where

import System.IO.Error
import System.Directory hiding (isSymbolicLink)
import Control.Monad
import System.FilePath
import Control.Applicative
import Control.Concurrent
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Maybe
import Prelude

#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#else
import qualified System.Posix as Posix
import Utility.SafeCommand
import Control.Monad.IfElse
#endif

import Utility.PosixFiles
import Utility.Tmp
import Utility.Exception
import Utility.Monad
import Utility.Applicative

dirCruft :: FilePath -> Bool
dirCruft "." = True
dirCruft ".." = True
dirCruft _ = False

{- Lists the contents of a directory.
 - Unlike getDirectoryContents, paths are not relative to the directory. -}
dirContents :: FilePath -> IO [FilePath]
dirContents d = map (d </>) . filter (not . dirCruft) <$> getDirectoryContents d

{- Gets files in a directory, and then its subdirectories, recursively,
 - and lazily.
 -
 - Does not follow symlinks to other subdirectories.
 -
 - When the directory does not exist, no exception is thrown,
 - instead, [] is returned. -}
dirContentsRecursive :: FilePath -> IO [FilePath]
dirContentsRecursive = dirContentsRecursiveSkipping (const False) True

{- Skips directories whose basenames match the skipdir. -}
dirContentsRecursiveSkipping :: (FilePath -> Bool) -> Bool -> FilePath -> IO [FilePath]
dirContentsRecursiveSkipping skipdir followsubdirsymlinks topdir = go [topdir]
  where
	go [] = return []
	go (dir:dirs)
		| skipdir (takeFileName dir) = go dirs
		| otherwise = unsafeInterleaveIO $ do
			(files, dirs') <- collect [] []
				=<< catchDefaultIO [] (dirContents dir)
			files' <- go (dirs' ++ dirs)
			return (files ++ files')
	collect files dirs' [] = return (reverse files, reverse dirs')
	collect files dirs' (entry:entries)
		| dirCruft entry = collect files dirs' entries
		| otherwise = do
			let skip = collect (entry:files) dirs' entries
			let recurse = collect files (entry:dirs') entries
			ms <- catchMaybeIO $ getSymbolicLinkStatus entry
			case ms of
				(Just s) 
					| isDirectory s -> recurse
					| isSymbolicLink s && followsubdirsymlinks ->
						ifM (doesDirectoryExist entry)
							( recurse
							, skip
							)
				_ -> skip

{- Gets the directory tree from a point, recursively and lazily,
 - with leaf directories **first**, skipping any whose basenames
 - match the skipdir. Does not follow symlinks. -}
dirTreeRecursiveSkipping :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
dirTreeRecursiveSkipping skipdir topdir = go [] [topdir]
  where
	go c [] = return c
	go c (dir:dirs)
		| skipdir (takeFileName dir) = go c dirs
		| otherwise = unsafeInterleaveIO $ do
			subdirs <- go c
				=<< filterM (isDirectory <$$> getSymbolicLinkStatus)
				=<< catchDefaultIO [] (dirContents dir)
			go (subdirs++[dir]) dirs

{- Moves one filename to another.
 - First tries a rename, but falls back to moving across devices if needed. -}
moveFile :: FilePath -> FilePath -> IO ()
moveFile src dest = tryIO (rename src dest) >>= onrename
  where
	onrename (Right _) = noop
	onrename (Left e)
		| isPermissionError e = rethrow
		| isDoesNotExistError e = rethrow
		| otherwise = viaTmp mv dest ""
	  where
		rethrow = throwM e

		mv tmp _ = do
		-- copyFile is likely not as optimised as
		-- the mv command, so we'll use the command.
		--
		-- But, while Windows has a "mv", it does not seem very
		-- reliable, so use copyFile there.
#ifndef mingw32_HOST_OS	
			-- If dest is a directory, mv would move the file
			-- into it, which is not desired.
			whenM (isdir dest) rethrow
			ok <- boolSystem "mv" [Param "-f", Param src, Param tmp]
			let e' = e
#else
			r <- tryIO $ copyFile src tmp
			let (ok, e') = case r of
				Left err -> (False, err)
				Right _ -> (True, e)
#endif
			unless ok $ do
				-- delete any partial
				_ <- tryIO $ removeFile tmp
				throwM e'

#ifndef mingw32_HOST_OS	
	isdir f = do
		r <- tryIO $ getFileStatus f
		case r of
			(Left _) -> return False
			(Right s) -> return $ isDirectory s
#endif

{- Removes a file, which may or may not exist, and does not have to
 - be a regular file.
 -
 - Note that an exception is thrown if the file exists but
 - cannot be removed. -}
nukeFile :: FilePath -> IO ()
nukeFile file = void $ tryWhenExists go
  where
#ifndef mingw32_HOST_OS
	go = removeLink file
#else
	go = removeFile file
#endif

#ifndef mingw32_HOST_OS
data DirectoryHandle = DirectoryHandle IsOpen Posix.DirStream
#else
data DirectoryHandle = DirectoryHandle IsOpen Win32.HANDLE Win32.FindData (MVar ())
#endif

type IsOpen = MVar () -- full when the handle is open

openDirectory :: FilePath -> IO DirectoryHandle
openDirectory path = do
#ifndef mingw32_HOST_OS
	dirp <- Posix.openDirStream path
	isopen <- newMVar ()
	return (DirectoryHandle isopen dirp)
#else
	(h, fdat) <- Win32.findFirstFile (path </> "*")
	-- Indicate that the fdat contains a filename that readDirectory
	-- has not yet returned, by making the MVar be full.
	-- (There's always at least a "." entry.)
	alreadyhave <- newMVar ()
	isopen <- newMVar ()
	return (DirectoryHandle isopen h fdat alreadyhave)
#endif

closeDirectory :: DirectoryHandle -> IO ()
#ifndef mingw32_HOST_OS
closeDirectory (DirectoryHandle isopen dirp) =
	whenOpen isopen $
		Posix.closeDirStream dirp
#else
closeDirectory (DirectoryHandle isopen h _ alreadyhave) =
	whenOpen isopen $ do
		_ <- tryTakeMVar alreadyhave
		Win32.findClose h
#endif
  where
	whenOpen :: IsOpen -> IO () -> IO ()
	whenOpen mv f = do
		v <- tryTakeMVar mv
		when (isJust v) f

{- |Reads the next entry from the handle. Once the end of the directory
is reached, returns Nothing and automatically closes the handle.
-}
readDirectory :: DirectoryHandle -> IO (Maybe FilePath)
#ifndef mingw32_HOST_OS
readDirectory hdl@(DirectoryHandle _ dirp) = do
	e <- Posix.readDirStream dirp
	if null e
		then do
			closeDirectory hdl
			return Nothing
		else return (Just e)
#else
readDirectory hdl@(DirectoryHandle _ h fdat mv) = do
	-- If the MVar is full, then the filename in fdat has
	-- not yet been returned. Otherwise, need to find the next
	-- file.
	r <- tryTakeMVar mv
	case r of
		Just () -> getfn
		Nothing -> do
			more <- Win32.findNextFile h fdat
			if more
				then getfn
				else do
					closeDirectory hdl
					return Nothing
  where
	getfn = do
		filename <- Win32.getFindDataFileName fdat
		return (Just filename)
#endif

-- True only when directory exists and contains nothing.
-- Throws exception if directory does not exist.
isDirectoryEmpty :: FilePath -> IO Bool
isDirectoryEmpty d = bracket (openDirectory d) closeDirectory check
  where
	check h = do
		v <- readDirectory h
		case v of
			Nothing -> return True
			Just f
				| not (dirCruft f) -> return False
				| otherwise -> check h
