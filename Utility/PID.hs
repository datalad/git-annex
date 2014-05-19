{- process ids
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.PID where

#ifndef mingw32_HOST_OS
import System.Posix.Types (ProcessID)
import System.Posix.Process (getProcessID)
#else
import System.Win32.Process (ProcessId)
import System.Win32.Process.Current (getCurrentProcessId)
#endif

#ifndef mingw32_HOST_OS
type PID = ProcessID
#else
type PID = ProcessId
#endif

getPID :: IO PID
#ifndef mingw32_HOST_OS
getPID = getProcessID
#else
getPID = getCurrentProcessId
#endif
