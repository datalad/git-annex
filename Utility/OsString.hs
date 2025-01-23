{- OsString manipulation. Or ByteString when not built with OsString.
 - Import qualified.
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.OsString (
	module X,
	length
) where

#ifdef WITH_OSPATH
import System.OsString as X hiding (length)
import qualified System.OsString
import qualified Data.ByteString as B
import Utility.OsPath

{- Avoid System.OsString.length, which returns the number of code points on
 - windows. This is the number of bytes. -}
length :: System.OsString.OsString -> Int
length = B.length . fromOsString
#else
import Data.ByteString as X hiding (length)
import Data.ByteString (length)
#endif
