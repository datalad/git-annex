{- Windows processes
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.WinProcess where

import Utility.PID

foreign import ccall unsafe "terminatepid"
	terminatePID :: PID -> IO ()
