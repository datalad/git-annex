{- openFd wrapper to support old versions of unix package.
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.OpenFd where

#ifndef mingw32_HOST_OS

import System.Posix.IO.ByteString
import System.Posix.Types

import Utility.RawFilePath

openFdWithMode :: RawFilePath -> OpenMode -> Maybe FileMode -> OpenFileFlags -> IO Fd
#if MIN_VERSION_unix(2,8,0)
openFdWithMode f openmode filemode flags = 
	openFd f openmode (flags { creat = filemode })
#else
openFdWithMode = openFd
#endif

#endif
