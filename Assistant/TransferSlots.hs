{- git-annex assistant transfer slots
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.TransferSlots where

import Control.Concurrent.STM

import Assistant.Common
import Utility.ThreadScheduler
import Utility.NotificationBroadcaster
import Assistant.Types.TransferSlots
import Assistant.DaemonStatus
import Annex.TransferrerPool
import Types.TransferrerPool
import Assistant.Types.TransferQueue
import Assistant.TransferQueue
import Assistant.Alert
import Assistant.Alert.Utility
import Assistant.Commits
import Assistant.Drop
import Types.Transfer
import Logs.Transfer
import Logs.Location
import qualified Git
import qualified Annex
import qualified Remote
import qualified Types.Remote as Remote
import Annex.Content
import Annex.Wanted
import Annex.StallDetection
import Utility.Batch
import Types.NumCopies

import Data.Either
import qualified Data.Map as M 
import qualified Control.Exception as E
import Control.Concurrent
import qualified Control.Concurrent.MSemN as MSemN
#ifndef mingw32_HOST_OS
import System.Posix.Process (getProcessGroupIDOf)
import System.Posix.Signals (signalProcessGroup, sigTERM, sigKILL)
#else
import System.Win32.Process (terminateProcessById)
#endif

type TransferGenerator = Assistant (Maybe (Transfer, TransferInfo, Transferrer -> Assistant ()))

{- Waits until a transfer slot becomes available, then runs a
 - TransferGenerator, and then runs the transfer action in its own thread. 
 -}
inTransferSlot :: RunTransferrer -> TransferGenerator -> Assistant ()
inTransferSlot rt gen = do
	flip MSemN.wait 1 <<~ transferSlots
	runTransferThread rt =<< gen

{- Runs a TransferGenerator, and its transfer action,
 - without waiting for a slot to become available. -}
inImmediateTransferSlot :: RunTransferrer -> TransferGenerator -> Assistant ()
inImmediateTransferSlot rt gen = do
	flip MSemN.signal (-1) <<~ transferSlots
	runTransferThread rt =<< gen

{- Runs a transfer action, in an already allocated transfer slot.
 - Once it finishes, frees the transfer slot.
 -
 - Note that the action is subject to being killed when the transfer
 - is canceled or paused.
 -
 - A PauseTransfer exception is handled by letting the action be killed,
 - then pausing the thread until a ResumeTransfer exception is raised,
 - then rerunning the action.
 -}
runTransferThread :: RunTransferrer -> Maybe (Transfer, TransferInfo, Transferrer -> Assistant ()) -> Assistant ()
runTransferThread _ Nothing = flip MSemN.signal 1 <<~ transferSlots
runTransferThread rt (Just (t, info, a)) = do
	d <- getAssistant id
	mkcheck <- checkNetworkConnections 
		<$> getAssistant daemonStatusHandle
	aio <- asIO1 a
	tid <- liftIO $ forkIO $ runTransferThread' mkcheck rt d aio
	updateTransferInfo t $ info { transferTid = Just tid }

runTransferThread' :: MkCheckTransferrer -> RunTransferrer -> AssistantData -> (Transferrer -> IO ()) -> IO ()
runTransferThread' mkcheck rt d run = go
  where
	go = catchPauseResume $ do
		p <- runAssistant d $ liftAnnex $ 
			Annex.getRead Annex.transferrerpool
		signalactonsvar <- runAssistant d $ liftAnnex $
			Annex.getRead Annex.signalactions
		withTransferrer' True signalactonsvar mkcheck rt p run
	pause = catchPauseResume $
		runEvery (Seconds 86400) noop
	{- Note: This must use E.try, rather than E.catch.
	 - When E.catch is used, and has called go in its exception
	 - handler, Control.Concurrent.throwTo will block sometimes
	 - when signaling. Using E.try avoids the problem. -}
	catchPauseResume a' = do
		r <- E.try a' :: IO (Either E.SomeException ())
		case r of
			Left e -> case E.fromException e of
				Just PauseTransfer -> pause
				Just ResumeTransfer -> go
				_ -> done
			_ -> done
	done = runAssistant d $ 
		flip MSemN.signal 1 <<~ transferSlots

{- By the time this is called, the daemonstatus's currentTransfers map should
 - already have been updated to include the transfer. -}
genTransfer :: Transfer -> TransferInfo -> TransferGenerator
genTransfer t info = case transferRemote info of
	Just remote -> ifM (unpluggedremovabledrive remote)
		( do
			-- optimisation, since the transfer would fail
			liftAnnex $ recordFailedTransfer t info
			void $ removeTransfer t
			return Nothing
		, ifM (liftAnnex $ shouldTransfer t info)
			( do
				qp <- liftAnnex $ coreQuotePath <$> Annex.getGitConfig
				debug [ "Transferring:" , describeTransfer qp t info ]
				notifyTransfer
				let sd = getStallDetection (transferDirection t) remote
				return $ Just (t, info, go remote sd)
			, do
				qp <- liftAnnex $ coreQuotePath <$> Annex.getGitConfig
				debug [ "Skipping unnecessary transfer:",
					describeTransfer qp t info ]
				void $ removeTransfer t
				finishedTransfer t (Just info)
				return Nothing
			)
		)
	_ -> return Nothing
  where
	direction = transferDirection t
	isdownload = direction == Download

	unpluggedremovabledrive remote = Git.repoIsLocalUnknown
		<$> liftAnnex (Remote.getRepo remote)

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
	 -
	 - If the process failed, it could have crashed,
	 - so remove the transfer from the list of current
	 - transfers, just in case it didn't stop
	 - in a way that lets the TransferWatcher do its
	 - usual cleanup. However, first check if something else is
	 - running the transfer, to avoid removing active transfers.
	 -}
	go remote sd transferrer = ifM (isRight <$> performTransfer sd AssistantLevel liftAnnex (transferRemote info) t info transferrer)
		( do
			case associatedFile info of
				AssociatedFile Nothing -> noop
				AssociatedFile (Just af) -> void $ 
					addAlert $ makeAlertFiller True $
						transferFileAlert direction True (fromRawFilePath af)
			unless isdownload $
				handleDrops
					("object uploaded to " ++ show remote)
					True (transferKey t)
					(associatedFile info)
					[mkVerifiedCopy RecentlyVerifiedCopy remote]
			void recordCommit
		, whenM (liftAnnex $ isNothing <$> checkTransfer t) $
			void $ removeTransfer t
		)

{- Called right before a transfer begins, this is a last chance to avoid
 - unnecessary transfers.
 -
 - For downloads, we obviously don't need to download if the already
 - have the object.
 -
 - Smilarly, for uploads, check if the remote is known to already have
 - the object.
 -
 - Also, uploads get queued to all remotes, in order of cost.
 - This may mean, for example, that an object is uploaded over the LAN
 - to a locally paired client, and once that upload is done, a more
 - expensive transfer remote no longer wants the object. (Since
 - all the clients have it already.) So do one last check if this is still
 - preferred content.
 -
 - We'll also do one last preferred content check for downloads. An
 - example of a case where this could be needed is if a download is queued
 - for a file that gets moved out of an archive directory -- but before
 - that download can happen, the file is put back in the archive.
 -}
shouldTransfer :: Transfer -> TransferInfo -> Annex Bool
shouldTransfer t info
	| transferDirection t == Download =
		(not <$> inAnnex key) <&&> wantGet NoLiveUpdate True (Just key) file
	| transferDirection t == Upload = case transferRemote info of
		Nothing -> return False
		Just r -> notinremote r
			<&&> wantGetBy NoLiveUpdate True (Just key) file (Remote.uuid r)
	| otherwise = return False
  where
	key = transferKey t
	file = associatedFile info

	{- Trust the location log to check if the remote already has
	 - the key. This avoids a roundtrip to the remote. -}
	notinremote r = notElem (Remote.uuid r) <$> loggedLocations key

{- Queue uploads of files downloaded to us, spreading them
 - out to other reachable remotes.
 -
 - Downloading a file may have caused a remote to not want it;
 - so check for drops from remotes.
 -
 - Uploading a file may cause the local repo, or some other remote to not
 - want it; handle that too.
 -}
finishedTransfer :: Transfer -> Maybe TransferInfo -> Assistant ()
finishedTransfer t (Just info)
	| transferDirection t == Download =
		whenM (liftAnnex $ inAnnex $ transferKey t) $ do
			dodrops False
			void $ queueTransfersMatching (/= transferUUID t)
				"newly received object"
				Later (transferKey t) (associatedFile info) Upload
	| otherwise = dodrops True
  where
	dodrops fromhere = do
		qp <- liftAnnex $ coreQuotePath <$> Annex.getGitConfig
		handleDrops
			("drop wanted after " ++ describeTransfer qp t info)
			fromhere (transferKey t) (associatedFile info) []
finishedTransfer _ _ = noop

{- Pause a running transfer. -}
pauseTransfer :: Transfer -> Assistant ()
pauseTransfer = cancelTransfer True

{- Cancel a running transfer. -}
cancelTransfer :: Bool -> Transfer -> Assistant ()
cancelTransfer pause t = do
	m <- getCurrentTransfers
	unless pause $
		{- remove queued transfer -}
		void $ dequeueTransfers $ equivilantTransfer t
	{- stop running transfer -}
	maybe noop stop (M.lookup t m)
  where
	stop info = do
		{- When there's a thread associated with the
		 - transfer, it's signaled first, to avoid it
		 - displaying any alert about the transfer having
		 - failed when the transfer process is killed. -}
		liftIO $ maybe noop signalthread $ transferTid info
		liftIO $ maybe noop killproc $ transferPid info
		if pause
			then void $ alterTransferInfo t $
				\i -> i { transferPaused = True }
			else void $ removeTransfer t
	signalthread tid
		| pause = throwTo tid PauseTransfer
		| otherwise = killThread tid
	killproc pid = void $ tryIO $ do
#ifndef mingw32_HOST_OS
		{- In order to stop helper processes like rsync,
		 - kill the whole process group of the process
		 - running the transfer. -}
		g <- getProcessGroupIDOf pid
		let signal sig = void $ tryIO $ signalProcessGroup sig g
		signal sigTERM
		threadDelay 50000 -- 0.05 second grace period
		signal sigKILL
#else
		terminateProcessById pid
#endif

{- Start or resume a transfer. -}
startTransfer :: Transfer -> Assistant ()
startTransfer t = do
	m <- getCurrentTransfers
	maybe startqueued go (M.lookup t m)
  where
	go info = maybe (start info) resume $ transferTid info
	startqueued = do
		is <- map snd <$> getMatchingTransfers (== t)
		maybe noop start $ headMaybe is
	resume tid = do
		alterTransferInfo t $ \i -> i { transferPaused = False }
		liftIO $ throwTo tid ResumeTransfer
	start info = do
		rt <- liftAnnex . mkRunTransferrer
			=<< liftIO getBatchCommandMaker
		inImmediateTransferSlot rt $
			genTransfer t info

getCurrentTransfers :: Assistant TransferMap
getCurrentTransfers = currentTransfers <$> getDaemonStatus

checkNetworkConnections :: DaemonStatusHandle -> MkCheckTransferrer
checkNetworkConnections dstatushandle = do
	dstatus <- atomically $ readTVar dstatushandle
	h <- newNotificationHandle False (networkConnectedNotifier dstatus)
	return $ not <$> checkNotification h
