{- File mode utilities.
 -
 - Copyright 2010-2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.FileMode (
	module Utility.FileMode,
	FileMode,
) where

import System.IO
import Control.Monad
import System.PosixCompat.Types
import Utility.PosixFiles
#ifndef mingw32_HOST_OS
import System.Posix.Files
import Control.Monad.IO.Class (liftIO)
#endif
import Control.Monad.IO.Class (MonadIO)
import Foreign (complement)
import Control.Monad.Catch

import Utility.Exception

{- Applies a conversion function to a file's mode. -}
modifyFileMode :: FilePath -> (FileMode -> FileMode) -> IO ()
modifyFileMode f convert = void $ modifyFileMode' f convert

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

{- Adds the specified FileModes to the input mode, leaving the rest
 - unchanged. -}
addModes :: [FileMode] -> FileMode -> FileMode
addModes ms m = combineModes (m:ms)

{- Removes the specified FileModes from the input mode. -}
removeModes :: [FileMode] -> FileMode -> FileMode
removeModes ms m = m `intersectFileModes` complement (combineModes ms)

writeModes :: [FileMode]
writeModes = [ownerWriteMode, groupWriteMode, otherWriteMode]

readModes :: [FileMode]
readModes = [ownerReadMode, groupReadMode, otherReadMode]

executeModes :: [FileMode]
executeModes = [ownerExecuteMode, groupExecuteMode, otherExecuteMode]

otherGroupModes :: [FileMode]
otherGroupModes = 
	[ groupReadMode, otherReadMode
	, groupWriteMode, otherWriteMode
	]

{- Removes the write bits from a file. -}
preventWrite :: FilePath -> IO ()
preventWrite f = modifyFileMode f $ removeModes writeModes

{- Turns a file's owner write bit back on. -}
allowWrite :: FilePath -> IO ()
allowWrite f = modifyFileMode f $ addModes [ownerWriteMode]

{- Turns a file's owner read bit back on. -}
allowRead :: FilePath -> IO ()
allowRead f = modifyFileMode f $ addModes [ownerReadMode]

{- Allows owner and group to read and write to a file. -}
groupSharedModes :: [FileMode]
groupSharedModes =
	[ ownerWriteMode, groupWriteMode
	, ownerReadMode, groupReadMode
	]

groupWriteRead :: FilePath -> IO ()
groupWriteRead f = modifyFileMode f $ addModes groupSharedModes

checkMode :: FileMode -> FileMode -> Bool
checkMode checkfor mode = checkfor `intersectFileModes` mode == checkfor

{- Checks if a file mode indicates it's a symlink. -}
isSymLink :: FileMode -> Bool
#ifdef mingw32_HOST_OS
isSymLink _ = False
#else
isSymLink = checkMode symbolicLinkMode
#endif

{- Checks if a file has any executable bits set. -}
isExecutable :: FileMode -> Bool
isExecutable mode = combineModes executeModes `intersectFileModes` mode /= 0

{- Runs an action without that pesky umask influencing it, unless the
 - passed FileMode is the standard one. -}
noUmask :: (MonadIO m, MonadMask m) => FileMode -> m a -> m a
#ifndef mingw32_HOST_OS
noUmask mode a
	| mode == stdFileMode = a
	| otherwise = withUmask nullFileMode a
#else
noUmask _ a = a
#endif

withUmask :: (MonadIO m, MonadMask m) => FileMode -> m a -> m a
#ifndef mingw32_HOST_OS
withUmask umask a = bracket setup cleanup go
  where
	setup = liftIO $ setFileCreationMask umask
	cleanup = liftIO . setFileCreationMask
	go _ = a
#else
withUmask _ a = a
#endif

combineModes :: [FileMode] -> FileMode
combineModes [] = 0
combineModes [m] = m
combineModes (m:ms) = foldl unionFileModes m ms

isSticky :: FileMode -> Bool
#ifdef mingw32_HOST_OS
isSticky _ = False
#else
isSticky = checkMode stickyMode

stickyMode :: FileMode
stickyMode = 512

setSticky :: FilePath -> IO ()
setSticky f = modifyFileMode f $ addModes [stickyMode]
#endif

{- Writes a file, ensuring that its modes do not allow it to be read
 - or written by anyone other than the current user,
 - before any content is written.
 -
 - When possible, this is done using the umask.
 -
 - On a filesystem that does not support file permissions, this is the same
 - as writeFile.
 -}
writeFileProtected :: FilePath -> String -> IO ()
writeFileProtected file content = writeFileProtected' file 
	(\h -> hPutStr h content)

writeFileProtected' :: FilePath -> (Handle -> IO ()) -> IO ()
writeFileProtected' file writer = withUmask 0o0077 $
	withFile file WriteMode $ \h -> do
		void $ tryIO $ modifyFileMode file $ removeModes otherGroupModes
		writer h
