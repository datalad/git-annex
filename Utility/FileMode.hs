{- File mode utilities.
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.FileMode (
	module Utility.FileMode,
	FileMode,
) where

import System.IO
import Control.Monad
import System.PosixCompat.Types
import System.PosixCompat.Files (unionFileModes, intersectFileModes, stdFileMode, nullFileMode, groupReadMode, ownerReadMode, ownerWriteMode, ownerExecuteMode, groupWriteMode, groupExecuteMode, otherReadMode, otherWriteMode, otherExecuteMode, fileMode)
#ifndef mingw32_HOST_OS
import System.PosixCompat.Files (setFileCreationMask)
#endif
import Control.Monad.IO.Class
import Foreign (complement)
import Control.Monad.Catch

import Utility.Exception
import Utility.FileSystemEncoding
import qualified Utility.RawFilePath as R

{- Applies a conversion function to a file's mode. -}
modifyFileMode :: RawFilePath -> (FileMode -> FileMode) -> IO ()
modifyFileMode f convert = void $ modifyFileMode' f convert

modifyFileMode' :: RawFilePath -> (FileMode -> FileMode) -> IO FileMode
modifyFileMode' f convert = do
	s <- R.getFileStatus f
	let old = fileMode s
	let new = convert old
	when (new /= old) $
		R.setFileMode f new
	return old

{- Runs an action after changing a file's mode, then restores the old mode. -}
withModifiedFileMode :: RawFilePath -> (FileMode -> FileMode) -> IO a -> IO a
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
	, groupExecuteMode, otherExecuteMode
	]

{- Removes the write bits from a file. -}
preventWrite :: RawFilePath -> IO ()
preventWrite f = modifyFileMode f $ removeModes writeModes

{- Turns a file's owner write bit back on. -}
allowWrite :: RawFilePath -> IO ()
allowWrite f = modifyFileMode f $ addModes [ownerWriteMode]

{- Turns a file's owner read bit back on. -}
allowRead :: RawFilePath -> IO ()
allowRead f = modifyFileMode f $ addModes [ownerReadMode]

{- Allows owner and group to read and write to a file. -}
groupSharedModes :: [FileMode]
groupSharedModes =
	[ ownerWriteMode, groupWriteMode
	, ownerReadMode, groupReadMode
	]

groupWriteRead :: RawFilePath -> IO ()
groupWriteRead f = modifyFileMode f $ addModes groupSharedModes

checkMode :: FileMode -> FileMode -> Bool
checkMode checkfor mode = checkfor `intersectFileModes` mode == checkfor

{- Checks if a file has any executable bits set. -}
isExecutable :: FileMode -> Bool
isExecutable mode = combineModes executeModes `intersectFileModes` mode /= 0

data ModeSetter = ModeSetter FileMode (RawFilePath -> IO ())

{- Runs an action which should create the file, passing it the desired
 - initial file mode. Then runs the ModeSetter's action on the file, which
 - can adjust the initial mode if umask prevented the file from being
 - created with the right mode. -}
applyModeSetter :: Maybe ModeSetter -> RawFilePath -> (Maybe FileMode -> IO a) -> IO a
applyModeSetter (Just (ModeSetter mode modeaction)) file a = do
	r <- a (Just mode)
	void $ tryIO $ modeaction file
	return r
applyModeSetter Nothing _ a = 
	a Nothing

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

getUmask :: IO FileMode
#ifndef mingw32_HOST_OS
getUmask = bracket setup cleanup return
  where
	setup = setFileCreationMask nullFileMode
	cleanup = setFileCreationMask
#else
getUmask = return nullFileMode
#endif

defaultFileMode :: IO FileMode
defaultFileMode = do
	umask <- getUmask
	return $ intersectFileModes (complement umask) stdFileMode

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

setSticky :: RawFilePath -> IO ()
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
writeFileProtected :: RawFilePath -> String -> IO ()
writeFileProtected file content = writeFileProtected' file 
	(\h -> hPutStr h content)

writeFileProtected' :: RawFilePath -> (Handle -> IO ()) -> IO ()
writeFileProtected' file writer = do
	h <- protectedOutput $ openFile (fromRawFilePath file) WriteMode
	void $ tryIO $ modifyFileMode file $ removeModes otherGroupModes
	writer h

protectedOutput :: IO a -> IO a
protectedOutput = withUmask 0o0077
