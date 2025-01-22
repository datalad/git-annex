{- Temporary directories
 -
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Tmp.Dir (
	withTmpDir,
	withTmpDirIn,
) where

import Control.Monad.IfElse
import System.FilePath
import System.Directory
import Control.Monad.IO.Class
#ifndef mingw32_HOST_OS
import System.Posix.Temp (mkdtemp)
#endif

import Utility.Exception
import Utility.Tmp (Template)
import Utility.OsPath
import Utility.FileSystemEncoding

{- Runs an action with a tmp directory located within the system's tmp
 - directory (or within "." if there is none), then removes the tmp
 - directory and all its contents. -}
withTmpDir :: (MonadMask m, MonadIO m) => Template -> (FilePath -> m a) -> m a
withTmpDir template a = do
	topleveltmpdir <- liftIO $ catchDefaultIO "." getTemporaryDirectory
#ifndef mingw32_HOST_OS
	-- Use mkdtemp to create a temp directory securely in /tmp.
	bracket
		(liftIO $ mkdtemp $ topleveltmpdir </> fromRawFilePath (fromOsPath template))
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
		makenewdir (tmpdir </> fromRawFilePath (fromOsPath template)) (0 :: Int)
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
	_ <- tryIO $ go tmpdir
	return ()
#else
	go tmpdir
#endif
  where
	go = removeDirectoryRecursive
