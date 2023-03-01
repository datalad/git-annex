{- Portability shim for basic operations on RawFilePaths.
 -
 - On unix, this makes syscalls using RawFilesPaths as efficiently as
 - possible.
 -
 - On Windows, filenames are in unicode, so RawFilePaths have to be
 - decoded. So this library will work, but less efficiently than using
 - FilePath would. However, this library also takes care to support long
 - filenames on Windows, by either using other libraries that do, or by
 - doing UNC-style conversion itself.
 -
 - Copyright 2019-2023 Joey Hess <id@joeyh.name>
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
	setOwnerAndGroup,
	rename,
	createNamedPipe,
	fileAccess,
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
-- System.PosixCompat does not handle UNC-style conversion itself,
-- so all uses of it library have to be pre-converted below. See
-- https://github.com/jacobstanley/unix-compat/issues/56
import qualified System.PosixCompat as P
import qualified System.Directory as D
import Utility.FileSystemEncoding
import Utility.Path.Windows

readSymbolicLink :: RawFilePath -> IO RawFilePath
readSymbolicLink f = toRawFilePath <$> P.readSymbolicLink (fromRawFilePath f)

createSymbolicLink :: RawFilePath -> RawFilePath -> IO ()
createSymbolicLink a b = do
	a' <- fromRawFilePath <$> convertToWindowsNativeNamespace a
	b' <- fromRawFilePath <$> convertToWindowsNativeNamespace b
	P.createSymbolicLink a' b'

createLink :: RawFilePath -> RawFilePath -> IO ()
createLink a b = do
	a' <- fromRawFilePath <$> convertToWindowsNativeNamespace a
	b' <- fromRawFilePath <$> convertToWindowsNativeNamespace b
	P.createLink a' b'

{- On windows, removeLink is not available, so only remove files,
 - not symbolic links. -}
removeLink :: RawFilePath -> IO ()
removeLink = D.removeFile . fromRawFilePath

getFileStatus :: RawFilePath -> IO FileStatus
getFileStatus p = P.getFileStatus . fromRawFilePath
	=<< convertToWindowsNativeNamespace p

getSymbolicLinkStatus :: RawFilePath -> IO FileStatus
getSymbolicLinkStatus p = P.getSymbolicLinkStatus . fromRawFilePath
	=<< convertToWindowsNativeNamespace p

doesPathExist :: RawFilePath -> IO Bool
doesPathExist = D.doesPathExist . fromRawFilePath

getCurrentDirectory :: IO RawFilePath
getCurrentDirectory = toRawFilePath <$> D.getCurrentDirectory

createDirectory :: RawFilePath -> IO ()
createDirectory = D.createDirectory . fromRawFilePath

setFileMode :: RawFilePath -> FileMode -> IO () 
setFileMode p m = do
	p' <- fromRawFilePath <$> convertToWindowsNativeNamespace p
	P.setFileMode p' m

{- Using renamePath rather than the rename provided in unix-compat
 - because of this bug https://github.com/jacobstanley/unix-compat/issues/56-}
rename :: RawFilePath -> RawFilePath -> IO ()
rename a b = D.renamePath (fromRawFilePath a) (fromRawFilePath b)

setOwnerAndGroup :: RawFilePath -> P.UserID -> P.GroupID -> IO ()
setOwnerAndGroup p u g = do
	p' <- fromRawFilePath <$> convertToWindowsNativeNamespace p
	P.setOwnerAndGroup p' u g

createNamedPipe :: RawFilePath -> FileMode -> IO ()
createNamedPipe p m = do
	p' <- fromRawFilePath <$> convertToWindowsNativeNamespace p
	P.createNamedPipe p' m

fileAccess :: RawFilePath -> Bool -> Bool -> Bool -> IO Bool
fileAccess p a b c = do
	p' <- fromRawFilePath <$> convertToWindowsNativeNamespace p
	P.fileAccess p' a b c
#endif
