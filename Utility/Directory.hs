{- directory traversal and manipulation
 -
 - Copyright 2011-2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Directory where

#ifdef WITH_OSPATH
import System.Directory.OsPath
#else
import Utility.SystemDirectory
#endif
import Control.Monad
import System.PosixCompat.Files (isDirectory, isSymbolicLink)
import Control.Applicative
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.FilePath.ByteString as P
import Data.Maybe
import Prelude

import Utility.OsPath
import Utility.Exception
import Utility.Monad
import qualified Utility.RawFilePath as R

dirCruft :: R.RawFilePath -> Bool
dirCruft "." = True
dirCruft ".." = True
dirCruft _ = False

{- Lists the contents of a directory.
 - Unlike getDirectoryContents, paths are not relative to the directory. -}
dirContents :: RawFilePath -> IO [RawFilePath]
dirContents d = 
	map (\p -> d P.</> fromOsPath p) 
		. filter (not . dirCruft . fromOsPath) 
		<$> getDirectoryContents (toOsPath d)

{- Gets files in a directory, and then its subdirectories, recursively,
 - and lazily.
 -
 - Does not follow symlinks to other subdirectories.
 -
 - Throws exception if the directory does not exist or otherwise cannot be
 - accessed. However, does not throw exceptions when subdirectories cannot
 - be accessed (the use of unsafeInterleaveIO would make it difficult to
 - trap such exceptions).
 -}
dirContentsRecursive :: RawFilePath -> IO [RawFilePath]
dirContentsRecursive = dirContentsRecursiveSkipping (const False) True

{- Skips directories whose basenames match the skipdir. -}
dirContentsRecursiveSkipping :: (RawFilePath -> Bool) -> Bool -> RawFilePath -> IO [RawFilePath]
dirContentsRecursiveSkipping skipdir followsubdirsymlinks topdir
	| skipdir (P.takeFileName topdir) = return []
	| otherwise = do
		-- Get the contents of the top directory outside of
		-- unsafeInterleaveIO, which allows throwing exceptions if
		-- it cannot be accessed.
		(files, dirs) <- collect [] []
			=<< dirContents topdir
		files' <- go dirs
		return (files ++ files')
  where
	go [] = return []
	go (dir:dirs)
		| skipdir (P.takeFileName dir) = go dirs
		| otherwise = unsafeInterleaveIO $ do
			(files, dirs') <- collect [] []
				=<< catchDefaultIO [] (dirContents dir)
			files' <- go (dirs' ++ dirs)
			return (files ++ files')
	
	collect :: [RawFilePath] -> [RawFilePath] -> [RawFilePath] -> IO ([RawFilePath], [RawFilePath])
	collect files dirs' [] = return (reverse files, reverse dirs')
	collect files dirs' (entry:entries)
		| dirCruft entry = collect files dirs' entries
		| otherwise = do
			let skip = collect (entry:files) dirs' entries
			let recurse = collect files (entry:dirs') entries
			ms <- catchMaybeIO $ R.getSymbolicLinkStatus entry
			case ms of
				(Just s) 
					| isDirectory s -> recurse
					| isSymbolicLink s && followsubdirsymlinks ->
						ifM (doesDirectoryExist (toOsPath entry))
							( recurse
							, skip
							)
				_ -> skip

{- Gets the directory tree from a point, recursively and lazily,
 - with leaf directories **first**, skipping any whose basenames
 - match the skipdir. Does not follow symlinks.
 -
 - Throws exception if the directory does not exist or otherwise cannot be
 - accessed. However, does not throw exceptions when subdirectories cannot
 - be accessed (the use of unsafeInterleaveIO would make it difficult to
 - trap such exceptions).
 -}
dirTreeRecursiveSkipping :: (RawFilePath -> Bool) -> RawFilePath -> IO [RawFilePath]
dirTreeRecursiveSkipping skipdir topdir
	| skipdir (P.takeFileName topdir) = return []
	| otherwise = do
		subdirs <- filterM isdir =<< dirContents topdir
		go [] subdirs
  where
	go c [] = return c
	go c (dir:dirs)
		| skipdir (P.takeFileName dir) = go c dirs
		| otherwise = unsafeInterleaveIO $ do
			subdirs <- go []
				=<< filterM isdir
				=<< catchDefaultIO [] (dirContents dir)
			go (subdirs++dir:c) dirs
	isdir p = isDirectory <$> R.getSymbolicLinkStatus p

{- When the action fails due to the directory not existing, returns []. -}
emptyWhenDoesNotExist :: IO [a] -> IO [a]
emptyWhenDoesNotExist a = tryWhenExists a >>= return . \case
	Just v -> v
	Nothing -> []

{- Use with an action that removes something, which may or may not exist.
 -
 - If an exception is thrown due to it not existing, it is ignored.
 -}
removeWhenExistsWith :: (a -> IO ()) -> a -> IO ()
removeWhenExistsWith f a = void $ tryWhenExists $ f a
