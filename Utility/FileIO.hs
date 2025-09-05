{- This is a subset of the functions provided by file-io.
 -
 - When not building with file-io, this provides equvilant
 - RawFilePath versions.
 -
 - Since Prelude exports many of these as well, this needs to be imported
 - qualified.
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utility.FileIO
(
	withFile,
	openFile,
	withBinaryFile,
	openBinaryFile,
	readFile,
	readFile',
	writeFile,
	writeFile',
	appendFile,
	appendFile',
	openTempFile,
) where

#ifdef WITH_OSPATH

#ifndef mingw32_HOST_OS
import System.File.OsPath
#else
-- On Windows, System.File.OsPath does not handle UNC-style conversion itself,
-- so that has to be done when calling it. See 
-- https://github.com/haskell/file-io/issues/39
import Utility.Path.Windows
import Utility.OsPath
import System.IO (IO, Handle, IOMode)
import Prelude (return)
import qualified System.File.OsPath as O
import qualified Data.ByteString as B
import Control.Applicative

withFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r 
withFile f m a = do
	f' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath f)
	O.withFile f' m a

openFile :: OsPath -> IOMode -> IO Handle
openFile f m = do
	f' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath f)
	O.openFile f' m

withBinaryFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r 
withBinaryFile f m a = do
	f' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath f)
	O.withBinaryFile f' m a

openBinaryFile :: OsPath -> IOMode -> IO Handle
openBinaryFile f m = do
	f' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath f)
	O.openBinaryFile f' m

readFile :: OsPath -> IO L.ByteString
readFile f = do
	f' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath f)
	O.readFile f'

readFile' :: OsPath -> IO B.ByteString
readFile' f = do
	f' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath f)
	O.readFile' f'

writeFile :: OsPath -> L.ByteString -> IO ()
writeFile f b = do
	f' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath f)
	O.writeFile f' b

writeFile' :: OsPath -> B.ByteString -> IO ()
writeFile' f b = do
	f' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath f)
	O.writeFile' f' b

appendFile :: OsPath -> L.ByteString -> IO ()
appendFile f b = do
	f' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath f)
	O.appendFile f' b

appendFile' :: OsPath -> B.ByteString -> IO ()
appendFile' f b = do
	f' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath f)
	O.appendFile' f' b

openTempFile :: OsPath -> OsPath -> IO (OsPath, Handle)
openTempFile p s = do
	p' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath p)
	(t, h) <- O.openTempFile p' s
	-- Avoid returning mangled path from convertToWindowsNativeNamespace
	let t' = p </> takeFileName t
	return (t', h)
#endif

#else
-- When not building with OsPath, export RawFilePath versions
-- instead.
import Utility.OsPath
import Utility.FileSystemEncoding
import System.IO (IO, Handle, IOMode)
import Prelude ((.), return)
import qualified System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

withFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r 
withFile = System.IO.withFile . fromRawFilePath

openFile :: OsPath -> IOMode -> IO Handle
openFile = System.IO.openFile . fromRawFilePath

withBinaryFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r 
withBinaryFile = System.IO.withBinaryFile . fromRawFilePath

openBinaryFile :: OsPath -> IOMode -> IO Handle
openBinaryFile = System.IO.openBinaryFile . fromRawFilePath

readFile :: OsPath -> IO L.ByteString
readFile = L.readFile . fromRawFilePath

readFile' :: OsPath -> IO B.ByteString
readFile' = B.readFile . fromRawFilePath

writeFile :: OsPath -> L.ByteString -> IO ()
writeFile = L.writeFile . fromRawFilePath

writeFile' :: OsPath -> B.ByteString -> IO ()
writeFile' = B.writeFile . fromRawFilePath

appendFile :: OsPath -> L.ByteString -> IO ()
appendFile = L.appendFile . fromRawFilePath

appendFile' :: OsPath -> B.ByteString -> IO ()
appendFile' = B.appendFile . fromRawFilePath

openTempFile :: OsPath -> OsPath -> IO (OsPath, Handle)
openTempFile p s = do
	(t, h) <- System.IO.openTempFile
		(fromRawFilePath p)
		(fromRawFilePath s)
	return (toRawFilePath t, h)
#endif
