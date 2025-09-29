{- openFd wrapper to support old versions of unix package.
 -
 - Copyright 2023-2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.OpenFd where

#ifndef mingw32_HOST_OS

import System.Posix.IO.ByteString
import System.Posix.Types
#if ! MIN_VERSION_unix(2,8,0)
import Control.Monad
#endif

import Utility.RawFilePath

newtype CloseOnExecFlag = CloseOnExecFlag Bool

openFdWithMode :: RawFilePath -> OpenMode -> Maybe FileMode -> OpenFileFlags -> CloseOnExecFlag -> IO Fd
openFdWithMode f openmode filemode flags (CloseOnExecFlag closeonexec) = do
#if MIN_VERSION_unix(2,8,0)
	openFd f openmode (flags { creat = filemode, cloexec = closeonexec })
#else
	fd <- openFd f openmode filemode flags
	when closeonexec $
		setFdOption fd CloseOnExec True
	return fd
#endif

#endif
