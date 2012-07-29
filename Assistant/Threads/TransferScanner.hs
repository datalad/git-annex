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
import Assistant.DaemonStatus
import Assistant.Alert
import Logs.Transfer
import Logs.Location
import qualified Remote
import Utility.ThreadScheduler
import qualified Git.LsFiles as LsFiles
import Command
import Annex.Content

thisThread :: ThreadName
thisThread = "TransferScanner"

{- This thread waits until a remote needs to be scanned, to find transfers
 - that need to be made, to keep data in sync.
 -}
transferScannerThread :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> TransferQueue -> IO ()
transferScannerThread st dstatus scanremotes transferqueue = do
	runEvery (Seconds 2) $ do
		r <- getScanRemote scanremotes
		liftIO $ debug thisThread ["starting scan of", show r]
		alertWhile dstatus (scanalert r) $
			scan st dstatus transferqueue r
		liftIO $ debug thisThread ["finished scan of", show r]
	where
		scanalert r = Alert
			{ alertClass = Activity
			, alertHeader = Just $ "Scanning " ++ Remote.name r
			, alertMessage = StringAlert $ unwords
				[ "Ensuring that ", Remote.name r
				, "is fully in sync." ]
			, alertBlockDisplay = True
			}

{- This is a naive scan through the git work tree.
 - 
 - The scan is blocked when the transfer queue gets too large. -}
scan :: ThreadState -> DaemonStatusHandle -> TransferQueue -> Remote -> IO ()
scan st dstatus transferqueue r = do
	g <- runThreadState st $ fromRepo id
	files <- LsFiles.inRepo [] g
	go files
	where
		go [] = return ()
		go (f:fs) = do
			v <- runThreadState st $ whenAnnexed check f
			case v of
				Nothing -> noop
				Just t -> do
					debug thisThread ["queuing", show t]
					enqueue f t
			go fs
			where
				check _ (key, _) = ifM (inAnnex key)
					( helper key Upload False =<< remotehas key
					, helper key Download True =<< remotehas key
					)
				helper key direction x y
					| x == y = return $
						Just $ Transfer direction u key
					| otherwise = return Nothing
				
		u = Remote.uuid r
		enqueue f t = queueTransferAt smallsize Later transferqueue dstatus (Just f) t r
		smallsize = 10

		{- Look directly in remote for the key when it's cheap;
		 - otherwise rely on the location log. -}
		remotehas key
			| Remote.hasKeyCheap r = (==)
				<$> pure (Right True)
				<*> Remote.hasKey r key
			| otherwise = elem
				<$> pure u
				<*> loggedLocations key
