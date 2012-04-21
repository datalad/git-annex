{- File mode utilities.
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.FileMode where

import Common

import Control.Exception (bracket)
import System.Posix.Types
import Foreign (complement)

combineModes :: [FileMode] -> FileMode
combineModes [] = undefined
combineModes [m] = m
combineModes (m:ms) = foldl unionFileModes m ms

{- Applies a conversion function to a file's mode. -}
modifyFileMode :: FilePath -> (FileMode -> FileMode) -> IO ()
modifyFileMode f convert = do
	_ <- modifyFileMode' f convert
	return ()
modifyFileMode' :: FilePath -> (FileMode -> FileMode) -> IO FileMode
modifyFileMode' f convert = do
	s <- getFileStatus f
	let old = fileMode s
	let new = convert old
	when (new /= old) $
		setFileMode f new
	return old

{- Runs an action after changing a file's mode, then restores the old mode. -}
withModifiedFileMode :: FilePath -> (FileMode -> FileMode) -> IO a -> IO a
withModifiedFileMode file convert a = bracket setup cleanup go
	where
		setup = modifyFileMode' file convert
		cleanup oldmode = modifyFileMode file (const oldmode)
		go _ = a

{- Removes a FileMode from a file.
 - For example, call with otherWriteMode to chmod o-w -}
unsetFileMode :: FilePath -> FileMode -> IO ()
unsetFileMode f m = modifyFileMode f $
	\cur -> cur `intersectFileModes` complement m

{- Removes the write bits from a file. -}
preventWrite :: FilePath -> IO ()
preventWrite f = unsetFileMode f $ combineModes writebits
	where
		writebits = [ownerWriteMode, groupWriteMode, otherWriteMode]

{- Turns a file's write bit back on. -}
allowWrite :: FilePath -> IO ()
allowWrite f = modifyFileMode f $
	\cur -> cur `unionFileModes` ownerWriteMode

{- Allows owner and group to read and write to a file. -}
groupWriteRead :: FilePath -> IO ()
groupWriteRead f = modifyFileMode f $ \cur -> combineModes
	[ cur
	, ownerWriteMode, groupWriteMode
	, ownerReadMode, groupReadMode
	]

{- Allows group to read a file. -}
groupRead :: FilePath -> IO ()
groupRead f = modifyFileMode f $ \cur -> combineModes
	[ cur
	, ownerReadMode, groupReadMode
	]

{- Allows all to read a file. -}
allRead :: FilePath -> IO ()
allRead f = modifyFileMode f $ \cur -> combineModes
	[ cur
	, ownerReadMode, groupReadMode, otherReadMode
	]

{- Checks if a file mode indicates it's a symlink. -}
isSymLink :: FileMode -> Bool
isSymLink mode = symbolicLinkMode `intersectFileModes` mode == symbolicLinkMode

{- Checks if a file has any executable bits set. -}
isExecutable :: FileMode -> Bool
isExecutable mode = combineModes ebits `intersectFileModes` mode /= 0
	where
		ebits = [ownerExecuteMode, groupExecuteMode, otherExecuteMode]
