{- directory traversal and manipulation
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Directory (
	module Utility.Directory,
	module Utility.SystemDirectory
) where

import System.IO.Error
import Control.Monad
import System.FilePath
import System.PosixCompat.Files
import Control.Applicative
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Maybe
import Prelude

#ifndef mingw32_HOST_OS
import Utility.SafeCommand
import Control.Monad.IfElse
#endif

import Utility.SystemDirectory
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
			subdirs <- go []
				=<< filterM (isDirectory <$$> getSymbolicLinkStatus)
				=<< catchDefaultIO [] (dirContents dir)
			go (subdirs++dir:c) dirs

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
