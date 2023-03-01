{- Temporary files.
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Tmp (
	Template,
	viaTmp,
	withTmpFile,
	withTmpFileIn,
	relatedTemplate,
	openTmpFileIn,
) where

import System.IO
import System.FilePath
import System.Directory
import Control.Monad.IO.Class
import System.IO.Error

import Utility.Exception
import Utility.FileSystemEncoding
import Utility.FileMode
import qualified Utility.RawFilePath as R

type Template = String

{- This is the same as openTempFile, except when there is an
 - error, it displays the template as well as the directory,
 - to help identify what call was responsible.
 -}
openTmpFileIn :: FilePath -> String -> IO (FilePath, Handle)
openTmpFileIn dir template = openTempFile dir template
	`catchIO` decoraterrror
  where
	decoraterrror e = throwM $
		let loc = ioeGetLocation e ++ " template " ++ template
		in annotateIOError e loc Nothing Nothing

{- Runs an action like writeFile, writing to a temp file first and
 - then moving it into place. The temp file is stored in the same
 - directory as the final file to avoid cross-device renames.
 -
 - While this uses a temp file, the file will end up with the same
 - mode as it would when using writeFile, unless the writer action changes
 - it.
 -}
viaTmp :: (MonadMask m, MonadIO m) => (FilePath -> v -> m ()) -> FilePath -> v -> m ()
viaTmp a file content = bracketIO setup cleanup use
  where
	(dir, base) = splitFileName file
	template = relatedTemplate (base ++ ".tmp")
	setup = do
		createDirectoryIfMissing True dir
		openTmpFileIn dir template
	cleanup (tmpfile, h) = do
		_ <- tryIO $ hClose h
		tryIO $ removeFile tmpfile
	use (tmpfile, h) = do
		let tmpfile' = toRawFilePath tmpfile
		-- Make mode the same as if the file were created usually,
		-- not as a temp file. (This may fail on some filesystems
		-- that don't support file modes well, so ignore
		-- exceptions.)
		_ <- liftIO $ tryIO $ R.setFileMode tmpfile' =<< defaultFileMode
		liftIO $ hClose h
		a tmpfile content
		liftIO $ R.rename tmpfile' (toRawFilePath file)

{- Runs an action with a tmp file located in the system's tmp directory
 - (or in "." if there is none) then removes the file. -}
withTmpFile :: (MonadIO m, MonadMask m) => Template -> (FilePath -> Handle -> m a) -> m a
withTmpFile template a = do
	tmpdir <- liftIO $ catchDefaultIO "." getTemporaryDirectory
	withTmpFileIn tmpdir template a

{- Runs an action with a tmp file located in the specified directory,
 - then removes the file.
 -
 - Note that the tmp file will have a file mode that only allows the
 - current user to access it.
 -}
withTmpFileIn :: (MonadIO m, MonadMask m) => FilePath -> Template -> (FilePath -> Handle -> m a) -> m a
withTmpFileIn tmpdir template a = bracket create remove use
  where
	create = liftIO $ openTmpFileIn tmpdir template
	remove (name, h) = liftIO $ do
		hClose h
		catchBoolIO (removeFile name >> return True)
	use (name, h) = a name h

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
