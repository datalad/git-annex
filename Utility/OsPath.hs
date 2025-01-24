{- OsPath utilities
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.OsPath (
	OsPath,
	OsString,
	RawFilePath,
	literalOsPath,
	toOsPath,
	fromOsPath,
	module X,
	getSearchPath,
	unsafeFromChar
) where

import Utility.FileSystemEncoding
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as S
#ifdef WITH_OSPATH
import System.OsPath as X hiding (OsPath, OsString, unsafeFromChar)
import System.OsPath
import "os-string" System.OsString.Internal.Types
import qualified System.FilePath.ByteString as PB
#else
import System.FilePath.ByteString as X hiding (RawFilePath, getSearchPath)
import System.FilePath.ByteString (getSearchPath)
import Data.ByteString (ByteString)
import Data.Char
import Data.Word
#endif

class OsPathConv t where
	toOsPath :: t -> OsPath
	fromOsPath :: OsPath -> t

instance OsPathConv FilePath where
	toOsPath = toOsPath . toRawFilePath
	fromOsPath = fromRawFilePath . fromOsPath

{- Used for string constants. -}
literalOsPath :: String -> OsPath
literalOsPath = toOsPath

#ifdef WITH_OSPATH
instance OsPathConv RawFilePath where
	toOsPath = bytesToOsPath . S.toShort
	fromOsPath = S.fromShort . bytesFromOsPath

instance OsPathConv ShortByteString where
	toOsPath = bytesToOsPath
	fromOsPath = bytesFromOsPath

{- Unlike System.OsString.fromBytes, on Windows this does not ensure a
 - valid USC-2LE encoding. The input ByteString must be in a valid encoding
 - already or uses of the OsPath will fail. -}
bytesToOsPath :: ShortByteString -> OsPath
#if defined(mingw32_HOST_OS)
bytesToOsPath = OsString . WindowsString
#else
bytesToOsPath = OsString . PosixString
#endif

bytesFromOsPath :: OsPath -> ShortByteString
#if defined(mingw32_HOST_OS)
bytesFromOsPath = getWindowsString . getOsString
#else
bytesFromOsPath = getPosixString . getOsString
#endif

{- For some reason not included in System.OsPath -}
getSearchPath :: IO [OsPath]
getSearchPath = map toOsPath <$> PB.getSearchPath

#else
{- When not building with WITH_OSPATH, use RawFilePath.
 -}
type OsPath = RawFilePath

type OsString = ByteString

instance OsPathConv RawFilePath where
	toOsPath = id
	fromOsPath = id

instance OsPathConv ShortByteString where
	toOsPath = S.fromShort
	fromOsPath = S.toShort

unsafeFromChar :: Char -> Word8
unsafeFromChar = fromIntegral . ord
#endif
