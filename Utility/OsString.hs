{- OsString utilities
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.OsString where

#ifdef WITH_OSSTRING

import Utility.RawFilePath

import "os-string" System.OsString.Internal.Types
import qualified Data.ByteString.Short as S

{- Unlike System.OsString.fromBytes, on Windows this does not ensure a
 - valid USC-2LE encoding. The input ByteString must be in a valid encoding
 - already or uses of the OsString will fail. -}
toOsString :: RawFilePath -> OsString
#if defined(mingw32_HOST_OS)
toOsString = OsString . WindowsString . S.toShort
#else
toOsString = OsString . PosixString . S.toShort
#endif

fromOsString :: OsString -> RawFilePath
#if defined(mingw32_HOST_OS)
fromOsString = S.fromShort . getWindowsString . getOsString
#else
fromOsString = S.fromShort . getPosixString . getOsString
#endif

#endif /* WITH_OSSTRING */
