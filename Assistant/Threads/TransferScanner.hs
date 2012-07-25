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
import Logs.Location
import Types.Remote
import qualified Remote
import Utility.ThreadScheduler
import qualified Git.LsFiles as LsFiles
import Command
import Annex.Content

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
		liftIO $ debug thisThread ["starting scan of", show r]
		needtransfer <- runThreadState st $ scan r
		forM_ needtransfer $ \(f, t) ->
			queueTransferAt smallsize Later transferqueue f t r
		liftIO $ debug thisThread ["finished scan of", show r]
	where
		smallsize = 10

{- This is a naive scan through the git work tree. -}
scan :: Remote -> Annex [(AssociatedFile, Transfer)]
scan r = do
	files <- inRepo $ LsFiles.inRepo []
	catMaybes <$> forM files (whenAnnexed go)
	where
		u = Remote.uuid r

		go file (key, _) =
			ifM (inAnnex key)
				( check Upload False =<< remotehas key
				, check Download True =<< remotehas key
				)
			where
				check direction x y
					| x == y = return $
						Just (Just file, Transfer direction u key)
					| otherwise = return Nothing

		{- Look directly in remote for the key when it's cheap;
		 - otherwise rely on the location log. -}
		remotehas key
			| Remote.hasKeyCheap r = (==)
				<$> pure (Right True)
				<*> Remote.hasKey r key
			| otherwise = elem
				<$> pure u
				<*> loggedLocations key
