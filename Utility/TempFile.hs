{- temp file functions
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.TempFile where

import Control.Exception (bracket)
import System.IO
#if 0
import System.Posix.Process
#endif
import System.Directory

import Utility.Exception
import Utility.Path
import System.FilePath

{- Runs an action like writeFile, writing to a temp file first and
 - then moving it into place. The temp file is stored in the same
 - directory as the final file to avoid cross-device renames. -}
viaTmp :: (FilePath -> String -> IO ()) -> FilePath -> String -> IO ()
#if 0
viaTmp a file content = do
	pid <- getProcessID
	let tmpfile = file ++ ".tmp" ++ show pid
	createDirectoryIfMissing True (parentDir file)
	a tmpfile content
	renameFile tmpfile file
#else
viaTmp = error "viaTMP TODO"
#endif

type Template = String

{- Runs an action with a temp file, then removes the file. -}
withTempFile :: Template -> (FilePath -> Handle -> IO a) -> IO a
withTempFile template a = bracket create remove use
  where
	create = do
		tmpdir <- catchDefaultIO "." getTemporaryDirectory
		openTempFile tmpdir template
	remove (name, handle) = do
		hClose handle
		catchBoolIO (removeFile name >> return True)
	use (name, handle) = a name handle

{- Runs an action with a temp directory, then removes the directory and
 - all its contents. -}
withTempDir :: Template -> (FilePath -> IO a) -> IO a
#if 0
withTempDir template = bracket create remove
  where
	remove = removeDirectoryRecursive
	create = do
		tmpdir <- catchDefaultIO "." getTemporaryDirectory
		createDirectoryIfMissing True tmpdir
		pid <- getProcessID
		makedir tmpdir (template ++ show pid) (0 :: Int)
	makedir tmpdir t n = do
		let dir = tmpdir </> t ++ "." ++ show n
		r <- tryIO $ createDirectory dir
		either (const $ makedir tmpdir t $ n + 1) (const $ return dir) r
#else
withTempDir = error "withTempDir TODO"
#endif
