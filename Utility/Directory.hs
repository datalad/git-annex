{- directory manipulation
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Directory where

import System.IO.Error
import System.Posix.Files
import System.Directory
import Control.Exception (throw)
import Control.Monad
import Control.Monad.IfElse
import System.FilePath
import Control.Applicative

import Utility.SafeCommand
import Utility.TempFile
import Utility.Exception
import Utility.Monad

{- Lists the contents of a directory.
 - Unlike getDirectoryContents, paths are not relative to the directory. -}
dirContents :: FilePath -> IO [FilePath]
dirContents d = map (d </>) . filter notcruft <$> getDirectoryContents d
	where
		notcruft "." = False
		notcruft ".." = False
		notcruft _ = True

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
					ok <- boolSystem "mv" [Param "-f",
						Param src, Param tmp]
					unless ok $ do
						-- delete any partial
						_ <- tryIO $ removeFile tmp
						rethrow
		isdir f = do
			r <- tryIO $ getFileStatus f
			case r of
				(Left _) -> return False
				(Right s) -> return $ isDirectory s
