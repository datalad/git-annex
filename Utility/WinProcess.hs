{- Windows processes
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.WinProcess where

import Utility.PID
import System.IO

terminatePID :: PID -> IO ()
terminatePID p = hPutStrLn stderr "terminating processes on windows is not currently working"

