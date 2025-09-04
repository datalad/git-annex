{- Opening files
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.OpenFile where

#ifndef mingw32_HOST_OS

import System.IO
import System.Posix.IO
import GHC.IO.FD
import GHC.IO.Handle.FD
import GHC.IO.Device

import Utility.OpenFd
import Utility.RawFilePath
import Utility.FileSystemEncoding

{- Usually, opening a Handle to a file that another thread also has open
 - for write is prevented, which avoids a lot of concurrency bugs especially
 - with lazy IO.
 - 
 - However, sometimes one thread is writing and another thread really wants
 - to read from the same file. This bypasses the usual locking, by claiming
 - that an opened FD is a Stream.
 -}
openFileBeingWritten :: RawFilePath -> IO Handle
openFileBeingWritten f = do
	fd <- openFdWithMode f ReadOnly Nothing defaultFileFlags (CloseOnExecFlag True)
	(fd', fdtype) <- mkFD (fromIntegral fd) ReadMode (Just (Stream, 0, 0)) False False
	mkHandleFromFD fd' fdtype (fromRawFilePath f) ReadMode False Nothing

#endif
