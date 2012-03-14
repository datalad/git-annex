{- File mode utilities.
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.FileMode where

import System.Posix.Files
import System.Posix.Types
import Foreign (complement)

{- Removes a FileMode from a file.
 - For example, call with otherWriteMode to chmod o-w -}
unsetFileMode :: FilePath -> FileMode -> IO ()
unsetFileMode f m = do
	s <- getFileStatus f
	setFileMode f $ fileMode s `intersectFileModes` complement m

{- Removes the write bits from a file. -}
preventWrite :: FilePath -> IO ()
preventWrite f = unsetFileMode f writebits
	where
		writebits = foldl unionFileModes ownerWriteMode
					[groupWriteMode, otherWriteMode]

{- Turns a file's write bit back on. -}
allowWrite :: FilePath -> IO ()
allowWrite f = do
	s <- getFileStatus f
	setFileMode f $ fileMode s `unionFileModes` ownerWriteMode

{- Checks if a file mode indicates it's a symlink. -}
isSymLink :: FileMode -> Bool
isSymLink mode = symbolicLinkMode `intersectFileModes` mode == symbolicLinkMode

{- Checks if a file has any executable bits set. -}
isExecutable :: FileMode -> Bool
isExecutable mode = ebits `intersectFileModes` mode /= 0
	where
		ebits = ownerExecuteMode `unionFileModes`
			groupExecuteMode `unionFileModes` otherExecuteMode
