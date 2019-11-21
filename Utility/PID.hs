{- process ids
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.PID (PID, getPID) where

#ifndef mingw32_HOST_OS
import System.Posix.Types (ProcessID)
import System.Posix.Process (getProcessID)
#else
import System.Win32.Process (ProcessId, getCurrentProcessId)
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
