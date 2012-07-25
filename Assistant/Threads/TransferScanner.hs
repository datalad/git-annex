{- git-annex assistant thread to scan remotes to find needed transfers
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.TransferScanner where

import Assistant.Common
import Assistant.ScanRemotes
import Assistant.TransferQueue
import Assistant.ThreadedMonad
import Logs.Transfer
import Types.Remote
import Utility.ThreadScheduler

thisThread :: ThreadName
thisThread = "TransferScanner"

{- This thread waits until a remote needs to be scanned, to find transfers
 - that need to be made, to keep data in sync.
 -
 - Remotes are scanned in the background; the scan is blocked when the
 - transfer queue gets too large.
 -}
transferScannerThread :: ThreadState -> ScanRemoteMap -> TransferQueue -> IO ()
transferScannerThread st scanremotes transferqueue = do
	runEvery (Seconds 2) $ do
		r <- getScanRemote scanremotes
		needtransfer <- scan st r
		forM_ needtransfer $ \(f, t) ->
			queueTransferAt smallsize Later transferqueue f t
	where
		smallsize = 10

{-  -}
scan :: ThreadState -> Remote -> IO [(AssociatedFile, Transfer)]
scan st r = do
	debug thisThread ["scanning", show r]
	return [] -- TODO 
