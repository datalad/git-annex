{- This is a subset of the functions provided by file-io.
 -
 - All functions have been modified to set the close-on-exec
 - flag to True.
 -
 - Also, functions that return a Handle have been modified to
 - use the locale encoding, working around this bug:
 - https://github.com/haskell/file-io/issues/45
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 - Copyright 2024 Julian Ospald
 -
 - License: BSD-3-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utility.FileIO.CloseOnExec
(
#ifdef WITH_OSPATH
	withFile,
	withFile',
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
#endif
) where

#ifdef WITH_OSPATH

import System.File.OsPath.Internal (withOpenFile', augmentError)
import qualified System.File.OsPath.Internal as I
import System.IO (IO, Handle, IOMode(..), hSetEncoding)
import GHC.IO.Encoding (getLocaleEncoding)
import System.OsPath (OsPath, OsString)
import Prelude (Bool(..), pure, either, (.), (>>=), ($))
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
#ifndef mingw32_HOST_OS
import System.Posix.IO
import Utility.Process
#endif

closeOnExec :: Bool
closeOnExec = True

withFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withFile osfp iomode act = (augmentError "withFile" osfp
    $ withOpenFileEncoding osfp iomode False False closeOnExec (try . act) True)
  >>= either ioError pure

withFile' :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withFile' osfp iomode act = (augmentError "withFile'" osfp
    $ withOpenFileEncoding osfp iomode False False closeOnExec (try . act) False)
  >>= either ioError pure

openFile :: OsPath -> IOMode -> IO Handle
openFile osfp iomode =  augmentError "openFile" osfp $
	withOpenFileEncoding osfp iomode False False closeOnExec pure False

withBinaryFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile osfp iomode act = (augmentError "withBinaryFile" osfp
    $ withOpenFileEncoding osfp iomode True False closeOnExec (try . act) True)
  >>= either ioError pure

openBinaryFile :: OsPath -> IOMode -> IO Handle
openBinaryFile osfp iomode = augmentError "openBinaryFile" osfp $
	 withOpenFileEncoding osfp iomode True False closeOnExec pure False

readFile :: OsPath -> IO BSL.ByteString
readFile fp = withFileNoEncoding' fp ReadMode BSL.hGetContents

readFile'
  :: OsPath -> IO BS.ByteString
readFile' fp = withFileNoEncoding fp ReadMode BS.hGetContents

writeFile :: OsPath -> BSL.ByteString -> IO ()
writeFile fp contents = withFileNoEncoding fp WriteMode (`BSL.hPut` contents)

writeFile'
  :: OsPath -> BS.ByteString -> IO ()
writeFile' fp contents = withFileNoEncoding fp WriteMode (`BS.hPut` contents)

appendFile :: OsPath -> BSL.ByteString -> IO ()
appendFile fp contents = withFileNoEncoding fp AppendMode (`BSL.hPut` contents)

appendFile'
  :: OsPath -> BS.ByteString -> IO ()
appendFile' fp contents = withFileNoEncoding fp AppendMode (`BS.hPut` contents)

{- Re-implementing openTempFile is difficult due to the current
 - structure of file-io. See this issue for discussion about improving
 - that: https://github.com/haskell/file-io/issues/44
 - So, instead this uses noCreateProcessWhile.
 - -}
openTempFile :: OsPath -> OsString -> IO (OsPath, Handle)
openTempFile tmp_dir template = do
#ifdef mingw32_HOST_OS
	(p, h) <- I.openTempFile tmp_dir template
	getLocaleEncoding >>= hSetEncoding h
	pure (p, h)
#else
	noCreateProcessWhile $ do
		(p, h) <- I.openTempFile tmp_dir template
		fd <- handleToFd h
		setFdOption fd CloseOnExec True
		h' <- fdToHandle fd
		getLocaleEncoding >>= hSetEncoding h'
		pure (p, h')
#endif

{- Wrapper around withOpenFile' that sets the locale encoding on the
 - Handle. -}
withOpenFileEncoding :: OsPath -> IOMode -> Bool -> Bool -> Bool -> (Handle -> IO r) -> Bool -> IO r
withOpenFileEncoding fp iomode binary existing cloExec action close_finally =
	withOpenFile' fp iomode binary existing cloExec action' close_finally
  where
	action' h = do
		getLocaleEncoding >>= hSetEncoding h
		action h

{- Variant of withFile above that does not have the overhead of setting the
 - locale encoding. Faster to use when the Handle is not used in a way that
 - needs any encoding. -}
withFileNoEncoding :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withFileNoEncoding osfp iomode act = (augmentError "withFile" osfp
    $ withOpenFile' osfp iomode False False closeOnExec (try . act) True)
  >>= either ioError pure

{- Variant of withFile' above that does not have the overhead of setting the
 - locale encoding. Faster to use when the Handle is not used in a way that
 - needs any encoding. -}
withFileNoEncoding' :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withFileNoEncoding' osfp iomode act = (augmentError "withFile'" osfp
    $ withOpenFile' osfp iomode False False closeOnExec (try . act) False)
  >>= either ioError pure

#endif
