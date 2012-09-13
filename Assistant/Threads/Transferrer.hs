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
import Locations.UserConfig

import System.Process (create_group)

thisThread :: ThreadName
thisThread = "Transferrer"

{- For now only one transfer is run at a time. -}
maxTransfers :: Int
maxTransfers = 1

{- Dispatches transfers from the queue. -}
transfererThread :: ThreadState -> DaemonStatusHandle -> TransferQueue -> TransferSlots -> NamedThread
transfererThread st dstatus transferqueue slots = thread $ go =<< readProgramFile
	where
		thread = NamedThread thisThread
		go program = forever $ inTransferSlot dstatus slots $
			maybe (return Nothing) (uncurry $ startTransfer st dstatus program)
				=<< getNextTransfer transferqueue dstatus notrunning
		{- Skip transfers that are already running. -}
		notrunning = isNothing . startedTime

{- By the time this is called, the daemonstatus's transfer map should
 - already have been updated to include the transfer. -}
startTransfer :: ThreadState -> DaemonStatusHandle -> FilePath -> Transfer -> TransferInfo -> TransferGenerator
startTransfer st dstatus program t info = case (transferRemote info, associatedFile info) of
	(Just remote, Just file) -> ifM (runThreadState st $ shouldTransfer t info)
		( do
			debug thisThread [ "Transferring:" , show t ]
			notifyTransfer dstatus
			return $ Just (t, info, transferprocess remote file)
		, do
			debug thisThread [ "Skipping unnecessary transfer:" , show t ]
			void $ removeTransfer dstatus t
			return Nothing
		)
	_ -> return Nothing
	where
		direction = transferDirection t
		isdownload = direction == Download

		transferprocess remote file = void $ do
			(_, _, _, pid)
				<- createProcess (proc program $ toCommand params)
					{ create_group = True }
			ok <- (==) ExitSuccess <$> waitForProcess pid
			when ok $ void $
				addAlert dstatus $
					makeAlertFiller ok $
						transferFileAlert direction ok file
			where
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
