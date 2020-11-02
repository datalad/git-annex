{- directory creating
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Directory.Create (
	createDirectoryUnder,
	createDirectoryUnder',
) where

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.IfElse
import System.IO.Error
import Data.Maybe
import qualified System.FilePath.ByteString as P
import Prelude

import Utility.SystemDirectory
import Utility.Path.AbsRel
import Utility.Exception
import Utility.FileSystemEncoding
import qualified Utility.RawFilePath as R
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
createDirectoryUnder :: RawFilePath -> RawFilePath -> IO ()
createDirectoryUnder topdir dir =
	createDirectoryUnder' topdir dir R.createDirectory

createDirectoryUnder'
	:: (MonadIO m, MonadCatch m)
	=> RawFilePath
	-> RawFilePath
	-> (RawFilePath -> m ())
	-> m ()
createDirectoryUnder' topdir dir0 mkdir = do
	p <- liftIO $ relPathDirToFile topdir dir0
	let dirs = P.splitDirectories p
	-- Catch cases where the dir is not beneath the topdir.
	-- If the relative path between them starts with "..",
	-- it's not. And on Windows, if they are on different drives,
	-- the path will not be relative.
	if headMaybe dirs == Just ".." || P.isAbsolute p
		then liftIO $ ioError $ customerror userErrorType
			("createDirectoryFrom: not located in " ++ fromRawFilePath topdir)
		-- If dir0 is the same as the topdir, don't try to create
		-- it, but make sure it does exist.
		else if null dirs
			then liftIO $ unlessM (doesDirectoryExist (fromRawFilePath topdir)) $
				ioError $ customerror doesNotExistErrorType
					"createDirectoryFrom: does not exist"
			else createdirs $
				map (topdir P.</>) (reverse (scanl1 (P.</>) dirs))
  where
	customerror t s = mkIOError t s Nothing (Just (fromRawFilePath dir0))

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
				liftIO $ unlessM (doesDirectoryExist (fromRawFilePath dir)) $
					ioError e
			| otherwise -> liftIO $ ioError e
