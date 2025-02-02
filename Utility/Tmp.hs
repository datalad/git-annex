{- Temporary files.
 -
 - Copyright 2010-2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Tmp (
	Template,
	viaTmp,
	withTmpFile,
	withTmpFileIn,
	openTmpFileIn,
	relatedTemplate,
	relatedTemplate',
) where

import System.IO
import Control.Monad.IO.Class
import System.IO.Error
import Data.Char
import qualified Data.ByteString as B

import Utility.Exception
import Utility.FileSystemEncoding
import Utility.FileMode
import qualified Utility.RawFilePath as R
import qualified Utility.FileIO as F
import Utility.OsPath
import Utility.SystemDirectory

type Template = OsString

{- This is the same as openTempFile, except when there is an
 - error, it displays the template as well as the directory,
 - to help identify what call was responsible.
 -}
openTmpFileIn :: OsPath -> Template -> IO (OsPath, Handle)
openTmpFileIn dir template = F.openTempFile dir template
	`catchIO` decoraterrror
  where
	decoraterrror e = throwM $
		let loc = ioeGetLocation e ++ " template " ++ decodeBS (fromOsPath template)
		in annotateIOError e loc Nothing Nothing

{- Runs an action like writeFile, writing to a temp file first and
 - then moving it into place. The temp file is stored in the same
 - directory as the final file to avoid cross-device renames.
 -
 - While this uses a temp file, the file will end up with the same
 - mode as it would when using writeFile, unless the writer action changes
 - it.
 -}
viaTmp :: (MonadMask m, MonadIO m) => (OsPath -> v -> m ()) -> OsPath -> v -> m ()
viaTmp a file content = bracketIO setup cleanup use
  where
	(dir, base) = splitFileName file
	template = relatedTemplate (fromOsPath base <> ".tmp")
	setup = do
		createDirectoryIfMissing True dir
		openTmpFileIn dir template
	cleanup (tmpfile, h) = do
		_ <- tryIO $ hClose h
		tryIO $ removeFile tmpfile
	use (tmpfile, h) = do
		let tmpfile' = fromOsPath tmpfile
		-- Make mode the same as if the file were created usually,
		-- not as a temp file. (This may fail on some filesystems
		-- that don't support file modes well, so ignore
		-- exceptions.)
		_ <- liftIO $ tryIO $
			R.setFileMode (fromOsPath tmpfile)
				=<< defaultFileMode
		liftIO $ hClose h
		a tmpfile content
		liftIO $ R.rename tmpfile' (fromOsPath file)

{- Runs an action with a tmp file located in the system's tmp directory
 - (or in "." if there is none) then removes the file. -}
withTmpFile :: (MonadIO m, MonadMask m) => Template -> (OsPath -> Handle -> m a) -> m a
withTmpFile template a = do
	tmpdir <- liftIO $ catchDefaultIO (literalOsPath ".") getTemporaryDirectory
	withTmpFileIn tmpdir template a

{- Runs an action with a tmp file located in the specified directory,
 - then removes the file.
 -
 - Note that the tmp file will have a file mode that only allows the
 - current user to access it.
 -}
withTmpFileIn :: (MonadIO m, MonadMask m) => OsPath -> Template -> (OsPath -> Handle -> m a) -> m a
withTmpFileIn tmpdir template a = bracket create remove use
  where
	create = liftIO $ openTmpFileIn tmpdir template
	remove (name, h) = liftIO $ do
		hClose h
		tryIO $ removeFile name
	use (name, h) = a name h

{- It's not safe to use a FilePath of an existing file as the template
 - for openTempFile, because if the FilePath is really long, the tmpfile
 - will be longer, and may exceed the maximum filename length.
 -
 - This generates a template that is never too long.
 -}
relatedTemplate :: RawFilePath -> Template
relatedTemplate = toOsPath . relatedTemplate'

relatedTemplate' :: RawFilePath -> RawFilePath
relatedTemplate' f
	| len > templateAddedLength = 
		{- Some filesystems like FAT have issues with filenames
		 - ending in ".", so avoid truncating a filename to end
		 - that way. -}
		B.dropWhileEnd (== dot) $
			truncateFilePath (len - templateAddedLength) f
	| otherwise = f
  where
	len = B.length f
	dot = fromIntegral (ord '.')

{- When a Template is used to create a temporary file, some random bytes
 - are appended to it. This is how many such bytes can be added, maximum.
 -
 - This needs to be as long or longer than the current implementation
 - of openTempFile, and some extra has been added to make it longer
 - than any likely implementation.
 -}
templateAddedLength :: Int
templateAddedLength = 20
