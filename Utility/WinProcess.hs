{- Windows processes
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.WinProcess where

import Utility.PID

terminatePID :: PID -> IO ()
terminatePID p = warning "terminating processes on windows is not currently working"
