{- temp file functions
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.TempFile where

import Control.Exception (bracket)
import System.IO
import System.Posix.Process hiding (executeFile)
import System.Directory

import Utility.Exception
import Utility.Path

{- Runs an action like writeFile, writing to a temp file first and
 - then moving it into place. The temp file is stored in the same
 - directory as the final file to avoid cross-device renames. -}
viaTmp :: (FilePath -> String -> IO ()) -> FilePath -> String -> IO ()
viaTmp a file content = do
	pid <- getProcessID
        let tmpfile = file ++ ".tmp" ++ show pid
	createDirectoryIfMissing True (parentDir file)
	a tmpfile content
	renameFile tmpfile file

type Template = String

{- Runs an action with a temp file, then removes the file. -}
withTempFile :: Template -> (FilePath -> Handle -> IO a) -> IO a
withTempFile template a = bracket create remove use
	where
		create = do
			tmpdir <- catchDefaultIO getTemporaryDirectory "."
			openTempFile tmpdir template
		remove (name, handle) = do
			hClose handle
			catchBoolIO (removeFile name >> return True)
		use (name, handle) = a name handle
