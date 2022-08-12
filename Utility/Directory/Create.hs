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
 - missing parent directories up to but not including a directory
 - from the first parameter.
 -
 - For example, createDirectoryUnder ["/tmp/foo"] "/tmp/foo/bar/baz"
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
 - The FilePaths can be relative, or absolute. 
 - They will be normalized as necessary.
 -
 - Note that, the second FilePath, if relative, is relative to the current
 - working directory.
 -}
createDirectoryUnder :: [RawFilePath] -> RawFilePath -> IO ()
createDirectoryUnder topdirs dir =
	createDirectoryUnder' topdirs dir R.createDirectory

createDirectoryUnder'
	:: (MonadIO m, MonadCatch m)
	=> [RawFilePath]
	-> RawFilePath
	-> (RawFilePath -> m ())
	-> m ()
createDirectoryUnder' topdirs dir0 mkdir = do
	relps <- liftIO $ forM topdirs $ \topdir -> relPathDirToFile topdir dir0
	let relparts = map P.splitDirectories relps
	-- Catch cases where dir0 is not beneath a topdir.
	-- If the relative path between them starts with "..",
	-- it's not. And on Windows, if they are on different drives,
	-- the path will not be relative.
	let notbeneath = \(_topdir, (relp, dirs)) -> 
		headMaybe dirs /= Just ".." && not (P.isAbsolute relp)
	case filter notbeneath $ zip topdirs (zip relps relparts) of
		((topdir, (_relp, dirs)):_)
			-- If dir0 is the same as the topdir, don't try to
			-- create it, but make sure it does exist.
			| null dirs ->
				liftIO $ unlessM (doesDirectoryExist (fromRawFilePath topdir)) $
					ioError $ customerror doesNotExistErrorType $
						"createDirectoryFrom: " ++ fromRawFilePath topdir ++ " does not exist"
			| otherwise -> createdirs $
					map (topdir P.</>) (reverse (scanl1 (P.</>) dirs))
		_ -> liftIO $ ioError $ customerror userErrorType
			("createDirectoryFrom: not located in " ++ unwords (map fromRawFilePath topdirs))
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
