{- git-annex assistant data transferrer thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Transferrer where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Assistant.TransferSlots
import Assistant.Alert
import Assistant.Commits
import Assistant.Drop
import Logs.Transfer
import Logs.Location
import Annex.Content
import qualified Remote
import Types.Key
import Locations.UserConfig

import System.Process (create_group)

{- Dispatches transfers from the queue. -}
transfererThread :: NamedThread
transfererThread = NamedThread "Transferr" $ do
	program <- liftIO readProgramFile
	forever $ inTransferSlot $
		maybe (return Nothing) (uncurry $ startTransfer program)
			=<< getNextTransfer notrunning
  where
	{- Skip transfers that are already running. -}
	notrunning = isNothing . startedTime

{- By the time this is called, the daemonstatus's transfer map should
 - already have been updated to include the transfer. -}
startTransfer :: FilePath -> Transfer -> TransferInfo -> Assistant (Maybe (Transfer, TransferInfo, Assistant ()))
startTransfer program t info = case (transferRemote info, associatedFile info) of
	(Just remote, Just file) -> ifM (liftAnnex $ shouldTransfer t info)
		( do
			debug [ "Transferring:" , show t ]
			notifyTransfer
			return $ Just (t, info, transferprocess remote file)
		, do
			debug [ "Skipping unnecessary transfer:" , show t ]
			void $ removeTransfer t
			return Nothing
		)
	_ -> return Nothing
  where
	direction = transferDirection t
	isdownload = direction == Download

	transferprocess remote file = void $ do
		(_, _, _, pid)
			<- liftIO $ createProcess (proc program $ toCommand params)
				{ create_group = True }
		{- Alerts are only shown for successful transfers.
		 - Transfers can temporarily fail for many reasons,
		 - so there's no point in bothering the user about
		 - those. The assistant should recover.
		 -
		 - After a successful upload, handle dropping it from
		 - here, if desired. In this case, the remote it was
		 - uploaded to is known to have it.
		 -
		 - Also, after a successful transfer, the location
		 - log has changed. Indicate that a commit has been
		 - made, in order to queue a push of the git-annex
		 - branch out to remotes that did not participate
		 - in the transfer.
		 -}
		whenM (liftIO $ (==) ExitSuccess <$> waitForProcess pid) $ do
			void $ addAlert $ makeAlertFiller True $
				transferFileAlert direction True file
			unless isdownload $
				handleDrops True (transferKey t)
					(associatedFile info)
					(Just remote)
			recordCommit
	  where
		params =
			[ Param "transferkey"
			, Param "--quiet"
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
