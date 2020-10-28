{- directory creating
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Directory.Create (
	createDirectoryUnder,
	createDirectoryUnder',
) where

import Control.Monad
import System.FilePath
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.IfElse
import System.IO.Error
import Data.Maybe
import Prelude

import Utility.SystemDirectory
import Utility.Path.AbsRel
import Utility.Exception
import Utility.FileSystemEncoding
import Utility.PartialPrelude

{- Like createDirectoryIfMissing True, but it will only create
 - missing parent directories up to but not including the directory
 - in the first parameter.
 -
 - For example, createDirectoryUnder "/tmp/foo" "/tmp/foo/bar/baz"
 - will create /tmp/foo/bar if necessary, but if /tmp/foo does not exist,
 - it will throw an exception.
 -
 - The exception thrown is the same that createDirectory throws if the
 - parent directory does not exist.
 -
 - If the second FilePath is not under the first
 - FilePath (or the same as it), it will fail with an exception
 - even if the second FilePath's parent directory already exists.
 -
 - Either or both of the FilePaths can be relative, or absolute.
 - They will be normalized as necessary.
 -
 - Note that, the second FilePath, if relative, is relative to the current
 - working directory, not to the first FilePath.
 -}
createDirectoryUnder :: FilePath -> FilePath -> IO ()
createDirectoryUnder topdir dir =
	createDirectoryUnder' topdir dir createDirectory

createDirectoryUnder'
	:: (MonadIO m, MonadCatch m)
	=> FilePath
	-> FilePath
	-> (FilePath -> m ())
	-> m ()
createDirectoryUnder' topdir dir0 mkdir = do
	p <- liftIO $ fromRawFilePath <$> relPathDirToFile
		(toRawFilePath topdir)
		(toRawFilePath dir0)
	let dirs = splitDirectories p
	-- Catch cases where the dir is not beneath the topdir.
	-- If the relative path between them starts with "..",
	-- it's not. And on Windows, if they are on different drives,
	-- the path will not be relative.
	if headMaybe dirs == Just ".." || isAbsolute p
		then liftIO $ ioError $ customerror userErrorType
			("createDirectoryFrom: not located in " ++ topdir)
		-- If dir0 is the same as the topdir, don't try to create
		-- it, but make sure it does exist.
		else if null dirs
			then liftIO $ unlessM (doesDirectoryExist topdir) $
				ioError $ customerror doesNotExistErrorType
					"createDirectoryFrom: does not exist"
			else createdirs $
				map (topdir </>) (reverse (scanl1 (</>) dirs))
  where
	customerror t s = mkIOError t s Nothing (Just dir0)

	createdirs [] = pure ()
	createdirs (dir:[]) = createdir dir (liftIO . ioError)
	createdirs (dir:dirs) = createdir dir $ \_ -> do
		createdirs dirs
		createdir dir (liftIO . ioError)

	-- This is the same method used by createDirectoryIfMissing,
	-- in particular the handling of errors that occur when the
	-- directory already exists. See its source for explanation
	-- of several subtleties.
	createdir dir notexisthandler = tryIO (mkdir dir) >>= \case
		Right () -> pure ()
		Left e
			| isDoesNotExistError e -> notexisthandler e
			| isAlreadyExistsError e || isPermissionError e ->
				liftIO $ unlessM (doesDirectoryExist dir) $
					ioError e
			| otherwise -> liftIO $ ioError e
