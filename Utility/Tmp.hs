{- Temporary files and directories.
 -
 - Copyright 2010-2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Tmp where

import System.IO
import System.Directory
import Control.Monad.IfElse
import System.FilePath
import Control.Monad.IO.Class
#ifndef mingw32_HOST_OS
import System.Posix.Temp (mkdtemp)
#endif

import Utility.Exception
import Utility.FileSystemEncoding
import Utility.PosixFiles

type Template = String

{- Runs an action like writeFile, writing to a temp file first and
 - then moving it into place. The temp file is stored in the same
 - directory as the final file to avoid cross-device renames. -}
viaTmp :: (MonadMask m, MonadIO m) => (FilePath -> String -> m ()) -> FilePath -> String -> m ()
viaTmp a file content = bracketIO setup cleanup use
  where
	(dir, base) = splitFileName file
	template = base ++ ".tmp"
	setup = do
		createDirectoryIfMissing True dir
		openTempFile dir template
	cleanup (tmpfile, h) = do
		_ <- tryIO $ hClose h
		tryIO $ removeFile tmpfile
	use (tmpfile, h) = do
		liftIO $ hClose h
		a tmpfile content
		liftIO $ rename tmpfile file

{- Runs an action with a tmp file located in the system's tmp directory
 - (or in "." if there is none) then removes the file. -}
withTmpFile :: (MonadIO m, MonadMask m) => Template -> (FilePath -> Handle -> m a) -> m a
withTmpFile template a = do
	tmpdir <- liftIO $ catchDefaultIO "." getTemporaryDirectory
	withTmpFileIn tmpdir template a

{- Runs an action with a tmp file located in the specified directory,
 - then removes the file. -}
withTmpFileIn :: (MonadIO m, MonadMask m) => FilePath -> Template -> (FilePath -> Handle -> m a) -> m a
withTmpFileIn tmpdir template a = bracket create remove use
  where
	create = liftIO $ openTempFile tmpdir template
	remove (name, h) = liftIO $ do
		hClose h
		catchBoolIO (removeFile name >> return True)
	use (name, h) = a name h

{- Runs an action with a tmp directory located within the system's tmp
 - directory (or within "." if there is none), then removes the tmp
 - directory and all its contents. -}
withTmpDir :: (MonadMask m, MonadIO m) => Template -> (FilePath -> m a) -> m a
withTmpDir template a = do
	topleveltmpdir <- liftIO $ catchDefaultIO "." getTemporaryDirectory
#ifndef mingw32_HOST_OS
	-- Use mkdtemp to create a temp directory securely in /tmp.
	bracket
		(liftIO $ mkdtemp $ topleveltmpdir </> template)
		removeTmpDir
		a
#else
	withTmpDirIn topleveltmpdir template a
#endif

{- Runs an action with a tmp directory located within a specified directory,
 - then removes the tmp directory and all its contents. -}
withTmpDirIn :: (MonadMask m, MonadIO m) => FilePath -> Template -> (FilePath -> m a) -> m a
withTmpDirIn tmpdir template = bracketIO create removeTmpDir
  where
	create = do
		createDirectoryIfMissing True tmpdir
		makenewdir (tmpdir </> template) (0 :: Int)
	makenewdir t n = do
		let dir = t ++ "." ++ show n
		catchIOErrorType AlreadyExists (const $ makenewdir t $ n + 1) $ do
			createDirectory dir
			return dir

{- Deletes the entire contents of the the temporary directory, if it
 - exists. -}
removeTmpDir :: MonadIO m => FilePath -> m ()
removeTmpDir tmpdir = liftIO $ whenM (doesDirectoryExist tmpdir) $ do
#if mingw32_HOST_OS
	-- Windows will often refuse to delete a file
	-- after a process has just written to it and exited.
	-- Because it's crap, presumably. So, ignore failure
	-- to delete the temp directory.
	_ <- tryIO $ removeDirectoryRecursive tmpdir
	return ()
#else
	removeDirectoryRecursive tmpdir
#endif

{- It's not safe to use a FilePath of an existing file as the template
 - for openTempFile, because if the FilePath is really long, the tmpfile
 - will be longer, and may exceed the maximum filename length.
 -
 - This generates a template that is never too long.
 - (Well, it allocates 20 characters for use in making a unique temp file,
 - anyway, which is enough for the current implementation and any
 - likely implementation.)
 -}
relatedTemplate :: FilePath -> FilePath
relatedTemplate f
	| len > 20 = truncateFilePath (len - 20) f
	| otherwise = f
  where
	len = length f
