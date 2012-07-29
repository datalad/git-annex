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
import Logs.Transfer
import Logs.Presence
import Logs.Location
import Annex.Content
import qualified Remote

import Data.Time.Clock.POSIX
import Data.Time.Clock
import qualified Data.Map as M

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
					transferThread st dstatus slots t info
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

{- A transfer is run in a separate thread, with a *copy* of the Annex
 - state. This is necessary to avoid blocking the rest of the assistant
 - on the transfer completing, and also to allow multiple transfers to run
 - at once. This requires GHC's threaded runtime to work!
 -
 - The copy of state  means that the transfer processes are responsible
 - for doing any necessary shutdown cleanups, and that the parent
 - thread's cache must be invalidated once a transfer completes, as
 - changes may have been made to the git-annex branch.
 -}
transferThread :: ThreadState -> DaemonStatusHandle -> TransferSlots -> Transfer -> TransferInfo -> IO ()
transferThread st dstatus slots t info = case (transferRemote info, associatedFile info) of
	(Nothing, _) -> noop
	(_, Nothing) -> noop
	(Just remote, Just file) -> do
		tid <- inTransferSlot slots st $
			transferprocess remote file
		now <- getCurrentTime
		adjustTransfers dstatus $
			M.insertWith' const t info
				{ startedTime = Just $ utcTimeToPOSIXSeconds now
				, transferTid = Just tid
				}
	where
		isdownload = transferDirection t == Download
		tofrom
			| isdownload = "from"
			| otherwise = "to"
		key = transferKey t

		transferprocess remote file = do
			showStart "copy" file
			showAction $ tofrom ++ " " ++ Remote.name remote
			ok <- runTransfer t (Just file) $
				if isdownload
					then getViaTmp key $
						Remote.retrieveKeyFile remote key (Just file)
					else do
						ok <- Remote.storeKey remote key $ Just file
						when ok $
							Remote.logStatus remote key InfoPresent
						return ok
			showEndResult ok
