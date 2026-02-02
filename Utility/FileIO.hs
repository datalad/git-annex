{- This is a subset of the functions provided by file-io, supplimented with
 - readFileString, writeFileString, and appendFileString.
 -
 - When building with file-io, all exported functions set the close-on-exec
 - flag.
 -
 - When not building with file-io, this provides equvilant
 - RawFilePath versions. Note that those versions do not currently
 - set the close-on-exec flag.
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

	readFileString,
	writeFileString,
	appendFileString,
) where

#ifdef WITH_OSPATH

#ifndef mingw32_HOST_OS
import Utility.FileIO.CloseOnExec
import Utility.FileIO.String
#else
import Utility.OsPath
import Utility.Path.Windows
import System.IO (IO, Handle, IOMode)
import Prelude (String, return)
import qualified Utility.FileIO.CloseOnExec as O
import qualified Utility.FileIO.String as O
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Applicative

withFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r 
withFile = O.withFile

openFile :: OsPath -> IOMode -> IO Handle
openFile = O.openFile

withBinaryFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r 
withBinaryFile = O.withBinaryFile

openBinaryFile :: OsPath -> IOMode -> IO Handle
openBinaryFile = O.openBinaryFile

readFile :: OsPath -> IO L.ByteString
readFile = O.readFile

readFile' :: OsPath -> IO B.ByteString
readFile' = O.readFile'

writeFile :: OsPath -> L.ByteString -> IO ()
writeFile = O.writeFile

writeFile' :: OsPath -> B.ByteString -> IO ()
writeFile' = O.writeFile'

appendFile :: OsPath -> L.ByteString -> IO ()
appendFile = O.appendFile

appendFile' :: OsPath -> B.ByteString -> IO ()
appendFile' = O.appendFile'

-- On Windows, System.File.OsPath does not handle UNC-style conversion itself
-- for this function, so that has to be done when calling it. See
-- https://github.com/haskell/file-io/issues/39
openTempFile :: OsPath -> OsPath -> IO (OsPath, Handle)
openTempFile p s = do
	p' <- toOsPath <$> convertToWindowsNativeNamespace (fromOsPath p)
	(t, h) <- O.openTempFile p' s
	-- Avoid returning mangled path from convertToWindowsNativeNamespace
	let t' = p </> takeFileName t
	return (t', h)

readFileString :: OsPath -> IO String
readFileString = O.readFileString

writeFileString :: OsPath -> String -> IO ()
writeFileString = O.writeFileString

appendFileString :: OsPath -> String -> IO ()
appendFileString = O.appendFileString
#endif

#else
-- RawFilePath versions
import Utility.OsPath
import Utility.FileSystemEncoding
import System.IO (IO, Handle, IOMode)
import Prelude (String, (.), return)
import qualified Prelude as P
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

readFileString :: OsPath -> IO String
readFileString = P.readFile . fromRawFilePath

writeFileString :: OsPath -> String -> IO ()
writeFileString = P.writeFile . fromRawFilePath

appendFileString :: OsPath -> String -> IO ()
appendFileString = P.appendFile . fromRawFilePath
#endif
