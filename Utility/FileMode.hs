{- File mode utilities.
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.FileMode where

import Common

import Control.Exception (bracket)
import Utility.Exception
import System.Posix.Types
import Foreign (complement)

{- Applies a conversion function to a file's mode. -}
modifyFileMode :: FilePath -> (FileMode -> FileMode) -> IO ()
modifyFileMode f convert = void $ modifyFileMode' f convert
#if 0
modifyFileMode' :: FilePath -> (FileMode -> FileMode) -> IO FileMode
modifyFileMode' f convert = do
	s <- getFileStatus f
	let old = fileMode s
	let new = convert old
	when (new /= old) $
		setFileMode f new
	return old
#else
modifyFileMode' = error "modifyFileMode' TODO"
#endif

{- Adds the specified FileModes to the input mode, leaving the rest
 - unchanged. -}
addModes :: [FileMode] -> FileMode -> FileMode
addModes ms m = combineModes (m:ms)

{- Removes the specified FileModes from the input mode. -}
removeModes :: [FileMode] -> FileMode -> FileMode
#if 0
removeModes ms m = m `intersectFileModes` complement (combineModes ms)
#else
removeModes = error "removeModes TODO"
#endif

{- Runs an action after changing a file's mode, then restores the old mode. -}
withModifiedFileMode :: FilePath -> (FileMode -> FileMode) -> IO a -> IO a
withModifiedFileMode file convert a = bracket setup cleanup go
  where
	setup = modifyFileMode' file convert
	cleanup oldmode = modifyFileMode file (const oldmode)
	go _ = a

writeModes :: [FileMode]
#if 0
writeModes = [ownerWriteMode, groupWriteMode, otherWriteMode]
#else
writeModes = []
#endif

readModes :: [FileMode]
#if 0
readModes = [ownerReadMode, groupReadMode, otherReadMode]
#else
readModes = []
#endif

executeModes :: [FileMode]
#if 0
executeModes = [ownerExecuteMode, groupExecuteMode, otherExecuteMode]
#else
executeModes = []
#endif

{- Removes the write bits from a file. -}
preventWrite :: FilePath -> IO ()
#if 0
preventWrite f = modifyFileMode f $ removeModes writeModes
#else
preventWrite _ = return ()
#endif

{- Turns a file's owner write bit back on. -}
allowWrite :: FilePath -> IO ()
#if 0
allowWrite f = modifyFileMode f $ addModes [ownerWriteMode]
#else
allowWrite _ = return ()
#endif

{- Allows owner and group to read and write to a file. -}
groupWriteRead :: FilePath -> IO ()
#if 0
groupWriteRead f = modifyFileMode f $ addModes
	[ ownerWriteMode, groupWriteMode
	, ownerReadMode, groupReadMode
	]
#else
groupWriteRead _ = return ()
#endif

#if 0
checkMode :: FileMode -> FileMode -> Bool
checkMode checkfor mode = checkfor `intersectFileModes` mode == checkfor
#endif

{- Checks if a file mode indicates it's a symlink. -}
isSymLink :: FileMode -> Bool
#if 0
isSymLink = checkMode symbolicLinkMode
#else
isSymLink _ = False
#endif

{- Checks if a file has any executable bits set. -}
isExecutable :: FileMode -> Bool
#if 0
isExecutable mode = combineModes executeModes `intersectFileModes` mode /= 0
#else
isExecutable _ = False
#endif

{- Runs an action without that pesky umask influencing it, unless the
 - passed FileMode is the standard one. -}
noUmask :: FileMode -> IO a -> IO a
#if 0
noUmask mode a
	| mode == stdFileMode = a
	| otherwise = bracket setup cleanup go
  where
	setup = setFileCreationMask nullFileMode
	cleanup = setFileCreationMask
	go _ = a
#else
noUmask _ a = a
#endif

combineModes :: [FileMode] -> FileMode
#if 0
combineModes [] = undefined
combineModes [m] = m
combineModes (m:ms) = foldl unionFileModes m ms
#else
combineModes _ = error "combineModes TODO"
#endif

stickyMode :: FileMode
stickyMode = 512

isSticky :: FileMode -> Bool
#if 0
isSticky = checkMode stickyMode
#else
isSticky _ = False
#endif

setSticky :: FilePath -> IO ()
setSticky f = modifyFileMode f $ addModes [stickyMode]

{- Writes a file, ensuring that its modes do not allow it to be read
 - by anyone other than the current user, before any content is written.
 -
 - On a filesystem that does not support file permissions, this is the same
 - as writeFile.
 -}
writeFileProtected :: FilePath -> String -> IO ()
#if 0
writeFileProtected file content = do
	h <- openFile file WriteMode
	void $ tryIO $
		modifyFileMode file $
			removeModes [groupReadMode, otherReadMode]
	hPutStr h content
	hClose h
#else
writeFileProtected = writeFile
#endif
