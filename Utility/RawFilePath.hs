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
) where

#ifndef mingw32_HOST_OS
import System.Posix.Files.ByteString
import System.Posix.ByteString.FilePath
#else
import qualified Data.ByteString as B
import qualified System.PosixCompat as P
import Utility.FileSystemEncoding

readSymbolicLink :: RawFilePath -> IO RawFilePath
readSymbolicLink f = toRawFilePath <$> P.readSymbolicLink (fromRawFilePath f)
#endif
