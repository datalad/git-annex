{- Portability shim for basic operations on RawFilePaths.
 -
 - On unix, this makes syscalls using RawFilesPaths as efficiently as
 - possible.
 -
 - On Windows, filenames are in unicode, so RawFilePaths have to be
 - decoded. So this library will work, but less efficiently than using
 - FilePath would.
 -
 - Copyright 2019-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.RawFilePath (
	RawFilePath,
	readSymbolicLink,
	createSymbolicLink,
	createLink,
	removeLink,
	getFileStatus,
	getSymbolicLinkStatus,
	doesPathExist,
	getCurrentDirectory,
	createDirectory,
	setFileMode,
) where

#ifndef mingw32_HOST_OS
import Utility.FileSystemEncoding (RawFilePath)
import System.Posix.Files.ByteString
import qualified System.Posix.Directory.ByteString as D

-- | Checks if a file or directory exists. Note that a dangling symlink
-- will be false.
doesPathExist :: RawFilePath -> IO Bool
doesPathExist = fileExist

getCurrentDirectory :: IO RawFilePath
getCurrentDirectory = D.getWorkingDirectory

createDirectory :: RawFilePath -> IO ()
createDirectory p = D.createDirectory p 0o777

#else
import System.PosixCompat (FileStatus, FileMode)
import qualified System.PosixCompat as P
import qualified System.PosixCompat.Files as F
import qualified System.Directory as D
import Utility.FileSystemEncoding

readSymbolicLink :: RawFilePath -> IO RawFilePath
readSymbolicLink f = toRawFilePath <$> P.readSymbolicLink (fromRawFilePath f)

createSymbolicLink :: RawFilePath -> RawFilePath -> IO ()
createSymbolicLink a b = P.createSymbolicLink
	(fromRawFilePath a)
	(fromRawFilePath b)

createLink :: RawFilePath -> RawFilePath -> IO ()
createLink a b = P.createLink
	(fromRawFilePath a)
	(fromRawFilePath b)

{- On windows, removeLink is not available, so only remove files,
 - not symbolic links. -}
removeLink :: RawFilePath -> IO ()
removeLink = D.removeFile . fromRawFilePath

getFileStatus :: RawFilePath -> IO FileStatus
getFileStatus = P.getFileStatus . fromRawFilePath

getSymbolicLinkStatus :: RawFilePath -> IO FileStatus
getSymbolicLinkStatus = P.getSymbolicLinkStatus . fromRawFilePath

doesPathExist :: RawFilePath -> IO Bool
doesPathExist = D.doesPathExist . fromRawFilePath

getCurrentDirectory :: IO RawFilePath
getCurrentDirectory = toRawFilePath <$> D.getCurrentDirectory

createDirectory :: RawFilePath -> IO ()
createDirectory = D.createDirectory . fromRawFilePath

setFileMode :: RawFilePath -> FileMode -> IO () 
setFileMode = F.setFileMode . fromRawFilePath
#endif
