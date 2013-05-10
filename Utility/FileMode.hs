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
#ifndef __WINDOWS__
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
#ifndef __WINDOWS__
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
#ifndef __WINDOWS__
writeModes = [ownerWriteMode, groupWriteMode, otherWriteMode]
#else
writeModes = []
#endif

readModes :: [FileMode]
#ifndef __WINDOWS__
readModes = [ownerReadMode, groupReadMode, otherReadMode]
#else
readModes = []
#endif

executeModes :: [FileMode]
#ifndef __WINDOWS__
executeModes = [ownerExecuteMode, groupExecuteMode, otherExecuteMode]
#else
executeModes = []
#endif

{- Removes the write bits from a file. -}
preventWrite :: FilePath -> IO ()
#ifndef __WINDOWS__
preventWrite f = modifyFileMode f $ removeModes writeModes
#else
preventWrite _ = return ()
#endif

{- Turns a file's owner write bit back on. -}
allowWrite :: FilePath -> IO ()
#ifndef __WINDOWS__
allowWrite f = modifyFileMode f $ addModes [ownerWriteMode]
#else
allowWrite _ = return ()
#endif

{- Allows owner and group to read and write to a file. -}
groupWriteRead :: FilePath -> IO ()
#ifndef __WINDOWS__
groupWriteRead f = modifyFileMode f $ addModes
	[ ownerWriteMode, groupWriteMode
	, ownerReadMode, groupReadMode
	]
#else
groupWriteRead _ = return ()
#endif

#ifndef __WINDOWS__
checkMode :: FileMode -> FileMode -> Bool
checkMode checkfor mode = checkfor `intersectFileModes` mode == checkfor
#endif

{- Checks if a file mode indicates it's a symlink. -}
isSymLink :: FileMode -> Bool
#ifndef __WINDOWS__
isSymLink = checkMode symbolicLinkMode
#else
isSymLink _ = False
#endif

{- Checks if a file has any executable bits set. -}
isExecutable :: FileMode -> Bool
#ifndef __WINDOWS__
isExecutable mode = combineModes executeModes `intersectFileModes` mode /= 0
#else
isExecutable _ = False
#endif

{- Runs an action without that pesky umask influencing it, unless the
 - passed FileMode is the standard one. -}
noUmask :: FileMode -> IO a -> IO a
#ifndef __WINDOWS__
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
#ifndef __WINDOWS__
combineModes [] = undefined
combineModes [m] = m
combineModes (m:ms) = foldl unionFileModes m ms
#else
combineModes _ = error "combineModes TODO"
#endif

stickyMode :: FileMode
stickyMode = 512

isSticky :: FileMode -> Bool
#ifndef __WINDOWS__
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
#ifndef __WINDOWS__
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
