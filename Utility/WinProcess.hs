{- Windows processes
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.WinProcess where

import Utility.PID

import System.Win32.Process
import Control.Exception (bracket)
import Control.Monad

terminatePID :: PID -> IO ()
terminatePID p = bracket 
	(openProcess pROCESS_TERMINATE False p)
	(void . c_closeProcess)
	(\h -> void $ c_TerminateProcess h 1)

foreign import ccall unsafe "windows.h TerminateProcess"
	c_TerminateProcess :: ProcessHandle -> Int -> IO Int

foreign import ccall unsafe "windows.h CloseHandle"
	c_closeProcess :: ProcessHandle -> IO Bool
