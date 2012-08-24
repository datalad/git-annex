{- git-annex assistant data transferrer thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Transferrer where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Assistant.TransferSlots
import Assistant.Alert
import Logs.Transfer
import Logs.Location
import Annex.Content
import qualified Remote
import Types.Key

import System.Process (create_group)

thisThread :: ThreadName
thisThread = "Transferrer"

{- For now only one transfer is run at a time. -}
maxTransfers :: Int
maxTransfers = 1

{- Dispatches transfers from the queue. -}
transfererThread :: ThreadState -> DaemonStatusHandle -> TransferQueue -> TransferSlots -> IO ()
transfererThread st dstatus transferqueue slots = go
	where
		go = getNextTransfer transferqueue dstatus notrunning >>= handle
		handle Nothing = go
		handle (Just (t, info)) = do
			ifM (runThreadState st $ shouldTransfer t info)
				( do
					debug thisThread [ "Transferring:" , show t ]
					notifyTransfer dstatus
					transferThread dstatus slots t info inTransferSlot
				, do
					debug thisThread [ "Skipping unnecessary transfer:" , show t ]
					-- getNextTransfer added t to the
					-- daemonstatus's transfer map.
					void $ removeTransfer dstatus t
				)
			go
		{- Skip transfers that are already running. -}
		notrunning i = startedTime i == Nothing

{- Checks if the file to download is already present, or the remote
 - being uploaded to isn't known to have the file. -}
shouldTransfer :: Transfer -> TransferInfo -> Annex Bool
shouldTransfer t info
	| transferDirection t == Download =
		not <$> inAnnex key
	| transferDirection t == Upload =
		{- Trust the location log to check if the
		 - remote already has the key. This avoids
		 - a roundtrip to the remote. -}
		case transferRemote info of
			Nothing -> return False
			Just remote -> 
				notElem (Remote.uuid remote)
					<$> loggedLocations key
	| otherwise = return False
	where
		key = transferKey t

{- A sepeate git-annex process is forked off to run a transfer,
 - running in its own process group. This allows killing it and all its
 - children if the user decides to cancel the transfer.
 -
 - A thread is forked off to run the process, and the thread
 - occupies one of the transfer slots. If all slots are in use, this will
 - block until one becomes available. The thread's id is also recorded in
 - the transfer info; the thread will also be killed when a transfer is
 - stopped, to avoid it displaying any alert about the transfer having
 - failed. -}
transferThread :: DaemonStatusHandle -> TransferSlots -> Transfer -> TransferInfo -> TransferSlotRunner -> IO ()
transferThread dstatus slots t info runner = case (transferRemote info, associatedFile info) of
	(Nothing, _) -> noop
	(_, Nothing) -> noop
	(Just remote, Just file) -> do
		tid <- runner slots $
			transferprocess remote file
		updateTransferInfo dstatus t $ info { transferTid = Just tid }
	where
		direction = transferDirection t
		isdownload = direction == Download

		transferprocess remote file = void $ do
			(_, _, _, pid)
				<- createProcess (proc command $ toCommand params)
					{ create_group = True }
			status <- waitForProcess pid
			addAlert dstatus $
				makeAlertFiller (status == ExitSuccess) $ 
					transferFileAlert direction file
			where
				command = "git-annex"
				params =
					[ Param "transferkey"
					, Param $ key2file $ transferKey t
					, Param $ if isdownload
						then "--from"
						else "--to"
					, Param $ Remote.name remote
					, Param "--file"
					, File file
					]
