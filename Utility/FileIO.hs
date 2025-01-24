{- File IO on OsPaths.
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
import qualified System.File.OsPath as O
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Applicative

withFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r 
withFile f m a = do
	f' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath f)
	O.withFile f' m a

openFile :: OsPath -> IOMode -> IO Handle
openFile f m = do
	f' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath f)
	O.openFile f' m

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
	O.openTempFile p' s
#endif

#else
-- When not building with OsPath, export FilePath versions
-- instead. However, functions still use ByteString for the
-- file content in that case, unlike the Strings used by the Prelude.
import Utility.OsPath
import System.IO (withFile, openFile, openTempFile, IO)
import qualified System.IO
import Data.ByteString.Lazy (readFile, writeFile, appendFile)
import qualified Data.ByteString as B

readFile' :: OsPath -> IO B.ByteString
readFile' = B.readFile

writeFile' :: OsPath -> B.ByteString -> IO ()
writeFile' = B.writeFile

appendFile' :: OsPath -> B.ByteString -> IO ()
appendFile' = B.appendFile
#endif
