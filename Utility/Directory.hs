{- directory manipulation
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Directory where

import System.IO.Error
import System.PosixCompat.Files
import System.Directory
import Control.Exception (throw)
import Control.Monad
import Control.Monad.IfElse
import System.FilePath
import Control.Applicative
import System.IO.Unsafe (unsafeInterleaveIO)

import Utility.SafeCommand
import Utility.Tmp
import Utility.Exception
import Utility.Monad

dirCruft :: FilePath -> Bool
dirCruft "." = True
dirCruft ".." = True
dirCruft _ = False

{- Lists the contents of a directory.
 - Unlike getDirectoryContents, paths are not relative to the directory. -}
dirContents :: FilePath -> IO [FilePath]
dirContents d = map (d </>) . filter (not . dirCruft) <$> getDirectoryContents d

{- Gets files in a directory, and then its subdirectories, recursively,
 - and lazily. If the directory does not exist, no exception is thrown,
 - instead, [] is returned. -}
dirContentsRecursive :: FilePath -> IO [FilePath]
dirContentsRecursive topdir = dirContentsRecursive' [topdir]

dirContentsRecursive' :: [FilePath] -> IO [FilePath]
dirContentsRecursive' [] = return []
dirContentsRecursive' (dir:dirs) = unsafeInterleaveIO $ do
	(files, dirs') <- collect [] [] =<< catchDefaultIO [] (dirContents dir)
	files' <- dirContentsRecursive' (dirs' ++ dirs)
	return (files ++ files')
  where
	collect files dirs' [] = return (reverse files, reverse dirs')
	collect files dirs' (entry:entries)
		| dirCruft entry = collect files dirs' entries
		| otherwise = do
			ifM (doesDirectoryExist entry)
				( collect files (entry:dirs') entries
				, collect (entry:files) dirs' entries
				)			

{- Moves one filename to another.
 - First tries a rename, but falls back to moving across devices if needed. -}
moveFile :: FilePath -> FilePath -> IO ()
moveFile src dest = tryIO (rename src dest) >>= onrename
  where
	onrename (Right _) = noop
	onrename (Left e)
		| isPermissionError e = rethrow
		| isDoesNotExistError e = rethrow
		| otherwise = do
			-- copyFile is likely not as optimised as
			-- the mv command, so we'll use the latter.
			-- But, mv will move into a directory if
			-- dest is one, which is not desired.
			whenM (isdir dest) rethrow
			viaTmp mv dest undefined
	  where
		rethrow = throw e
		mv tmp _ = do
			ok <- boolSystem "mv" [Param "-f", Param src, Param tmp]
			unless ok $ do
				-- delete any partial
				_ <- tryIO $ removeFile tmp
				rethrow

	isdir f = do
		r <- tryIO $ getFileStatus f
		case r of
			(Left _) -> return False
			(Right s) -> return $ isDirectory s

{- Removes a file, which may or may not exist, and does not have to
 - be a regular file.
 -
 - Note that an exception is thrown if the file exists but
 - cannot be removed. -}
nukeFile :: FilePath -> IO ()
#ifndef mingw32_HOST_OS
nukeFile = removeLink
#else
nukeFile = removeFile
#endif
