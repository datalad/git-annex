{- Portability shim around System.Posix.Files.ByteString and
 - System.Posix.Directory.ByteString
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

module Utility.RawFilePath (
	RawFilePath,
	readSymbolicLink,
	createSymbolicLink,
	getFileStatus,
	getSymbolicLinkStatus,
	doesPathExist,
	getCurrentDirectory,
) where

#ifndef mingw32_HOST_OS
import Utility.FileSystemEncoding (RawFilePath)
import System.Posix.Files.ByteString
import System.Posix.Directory.ByteString

-- | Checks if a file or directoy exists. Note that a dangling symlink
-- will be false.
doesPathExist :: RawFilePath -> IO Bool
doesPathExist = fileExist

getCurrentDirectory :: IO RawFilePath
getCurrentDirectory = getWorkingDirectory

#else
import qualified Data.ByteString as B
import System.PosixCompat (FileStatus)
import qualified System.PosixCompat as P
import qualified System.Directory as D
import Utility.FileSystemEncoding

readSymbolicLink :: RawFilePath -> IO RawFilePath
readSymbolicLink f = toRawFilePath <$> P.readSymbolicLink (fromRawFilePath f)

createSymbolicLink :: RawFilePath -> RawFilePath -> IO ()
createSymbolicLink a b = P.createSymbolicLink
	(fromRawFilePath a)
	(fromRawFilePath b)

getFileStatus :: RawFilePath -> IO FileStatus
getFileStatus = P.getFileStatus . fromRawFilePath

getSymbolicLinkStatus :: RawFilePath -> IO FileStatus
getSymbolicLinkStatus = P.getSymbolicLinkStatus . fromRawFilePath

doesPathExist :: RawFilePath -> IO Bool
doesPathExist = D.doesPathExist . fromRawFilePath

getCurrentDirectory :: IO RawFilePath
getCurrentDirectory = toRawFilePath <$> D.getCurrentDirectory
#endif
