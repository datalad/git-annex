{- OsPath utilities
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.OsPath (
	OsPath,
	OsString,
	toOsPath,
	fromOsPath,
) where

import Utility.FileSystemEncoding

#ifdef WITH_OSPATH
import System.OsPath
import "os-string" System.OsString.Internal.Types
import qualified Data.ByteString.Short as S
#if defined(mingw32_HOST_OS)
import GHC.IO (unsafePerformIO)
import System.OsString.Encoding.Internal (cWcharsToChars_UCS2)
import qualified System.OsString.Data.ByteString.Short.Word16 as BS16
#endif

toOsPath :: RawFilePath -> OsPath
#if defined(mingw32_HOST_OS)
-- On Windows, OsString contains a ShortByteString that is
-- utf-16 encoded. So have to convert the input to that.
-- This is relatively expensive.
toOsPath = unsafePerformIO . encodeFS . fromRawFilePath
#else
toOsPath = OsString . PosixString . S.toShort
#endif

fromOsPath :: OsPath -> RawFilePath
#if defined(mingw32_HOST_OS)
-- On Windows, OsString contains a ShortByteString that is
-- utf-16 encoded. So have to convert the input from that.
-- This is relatively expensive.
fromOsPath = toRawFilePath . cWcharsToChars_UCS2 . BS16.unpack . getWindowsString
#else
fromOsPath = S.fromShort . getPosixString . getOsString
#endif

#else
{- When not building with WITH_OSPATH, use FilePath. This allows
 - using functions from legacy FilePath libraries interchangeably with
 - newer OsPath libraries.
 -}
type OsPath = FilePath

type OsString = String

toOsPath :: RawFilePath -> OsPath
toOsPath = fromRawFilePath

fromOsPath :: OsPath -> RawFilePath
fromOsPath = toRawFilePath
#endif
