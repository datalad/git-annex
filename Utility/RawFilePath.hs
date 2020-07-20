{- Portability shim around System.Posix.Files.ByteString
 -
 - On unix, this makes syscalls using RawFilesPaths as efficiently as
 - possible.
 -
 - On Windows, filenames are in unicode, so RawFilePaths have to be
 - decoded. So this library will work, but less efficiently than using
 - FilePath would.
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.RawFilePath (
	RawFilePath,
	readSymbolicLink,
	getFileStatus,
	getSymbolicLinkStatus,
	doesPathExist,
) where

#ifndef mingw32_HOST_OS
import Utility.FileSystemEncoding (RawFilePath)
import System.Posix.Files.ByteString

-- | Checks if a file or directoy exists. Note that a dangling symlink
-- will be false.
doesPathExist :: RawFilePath -> IO Bool
doesPathExist = fileExist

#else
import qualified Data.ByteString as B
import System.PosixCompat (FileStatus)
import qualified System.PosixCompat as P
import qualified System.Directory as D
import Utility.FileSystemEncoding

readSymbolicLink :: RawFilePath -> IO RawFilePath
readSymbolicLink f = toRawFilePath <$> P.readSymbolicLink (fromRawFilePath f)

getFileStatus :: RawFilePath -> IO FileStatus
getFileStatus = P.getFileStatus . fromRawFilePath

getSymbolicLinkStatus :: RawFilePath -> IO FileStatus
getSymbolicLinkStatus = P.getSymbolicLinkStatus . fromRawFilePath

doesPathExist :: RawFilePath -> IO Bool
doesPathExist = D.doesPathExist . fromRawFilePath
#endif
