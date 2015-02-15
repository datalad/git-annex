{- Windows processes
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.WinProcess where

import Utility.PID

foreign import ccall unsafe "terminatepid"
	terminatePID :: PID -> IO ()
