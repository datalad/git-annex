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

import Utility.SafeCommand
import Utility.TempFile
import Utility.Exception

{- Moves one filename to another.
 - First tries a rename, but falls back to moving across devices if needed. -}
moveFile :: FilePath -> FilePath -> IO ()
moveFile src dest = tryIO (rename src dest) >>= onrename
	where
		onrename (Right _) = return ()
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
