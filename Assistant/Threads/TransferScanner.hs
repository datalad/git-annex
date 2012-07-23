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

{- This thread scans remotes, to find transfers that need to be made to
 - keep their data in sync. The transfers are queued with lot priority. -}
transferScannerThread :: ThreadState -> ScanRemoteMap -> TransferQueue -> IO ()
transferScannerThread st scanremotes transferqueue = do
	runEvery (Seconds 2) $ do
		r <- getScanRemote scanremotes
		needtransfer <- scan st r
		forM_ needtransfer $ \(f, t) ->
			queueLaterTransfer transferqueue f t

scan :: ThreadState -> Remote -> IO [(AssociatedFile, Transfer)]
scan st r = do
	debug thisThread ["scanning", show r]
	return [] -- TODO 
