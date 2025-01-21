{- OsPath utilities
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.OsPath where

#ifdef WITH_OSPATH

import Utility.RawFilePath

import System.OsPath
import "os-string" System.OsString.Internal.Types
import qualified Data.ByteString.Short as S

{- Unlike System.OsString.fromBytes, on Windows this does not ensure a
 - valid USC-2LE encoding. The input ByteString must be in a valid encoding
 - already or uses of the OsPath will fail. -}
toOsPath :: RawFilePath -> OsPath
#if defined(mingw32_HOST_OS)
toOsPath = OsString . WindowsString . S.toShort
#else
toOsPath = OsString . PosixString . S.toShort
#endif

fromOsPath :: OsPath -> RawFilePath
#if defined(mingw32_HOST_OS)
fromOsPath = S.fromShort . getWindowsString . getOsString
#else
fromOsPath = S.fromShort . getPosixString . getOsString
#endif

#endif /* WITH_OSPATH */
