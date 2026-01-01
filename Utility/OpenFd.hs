{- openFd wrapper
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

import Utility.RawFilePath

newtype CloseOnExecFlag = CloseOnExecFlag Bool

openFdWithMode :: RawFilePath -> OpenMode -> Maybe FileMode -> OpenFileFlags -> CloseOnExecFlag -> IO Fd
openFdWithMode f openmode filemode flags (CloseOnExecFlag closeonexec) = do
	openFd f openmode (flags { creat = filemode, cloexec = closeonexec })

#endif
