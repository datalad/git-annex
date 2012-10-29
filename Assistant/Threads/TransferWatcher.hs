{- git-annex assistant transfer watching thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.TransferWatcher where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Assistant.Drop
import Annex.Content
import Logs.Transfer
import Utility.DirWatcher
import Utility.Types.DirWatcher
import qualified Remote

import Control.Concurrent

thisThread :: ThreadName
thisThread = "TransferWatcher"

{- This thread watches for changes to the gitAnnexTransferDir,
 - and updates the DaemonStatus's map of ongoing transfers. -}
transferWatcherThread :: ThreadState -> DaemonStatusHandle -> TransferQueue -> NamedThread
transferWatcherThread st dstatus transferqueue = thread $ liftIO $ do
	g <- runThreadState st gitRepo
	let dir = gitAnnexTransferDir g
	createDirectoryIfMissing True dir
	let hook a = Just $ runHandler st dstatus transferqueue a
	let hooks = mkWatchHooks
		{ addHook = hook onAdd
		, delHook = hook onDel
		, modifyHook = hook onModify
		, errHook = hook onErr
		}
	void $ watchDir dir (const False) hooks id
	brokendebug thisThread ["watching for transfers"]
	where
		thread = NamedThread thisThread

type Handler = ThreadState -> DaemonStatusHandle -> TransferQueue -> FilePath -> Maybe FileStatus -> IO ()

{- Runs an action handler.
 -
 - Exceptions are ignored, otherwise a whole thread could be crashed.
 -}
runHandler :: ThreadState -> DaemonStatusHandle -> TransferQueue -> Handler -> FilePath -> Maybe FileStatus -> IO ()
runHandler st dstatus transferqueue handler file filestatus = void $
        either print (const noop) =<< tryIO go
        where
                go = handler st dstatus transferqueue file filestatus

{- Called when there's an error with inotify. -}
onErr :: Handler
onErr _ _ _ msg _ = error msg

{- Called when a new transfer information file is written. -}
onAdd :: Handler
onAdd st dstatus _ file _ = case parseTransferFile file of
	Nothing -> noop
	Just t -> go t =<< runThreadState st (checkTransfer t)
	where
		go _ Nothing = noop -- transfer already finished
		go t (Just info) = do
			brokendebug thisThread
				[ "transfer starting:"
				, show t
				]
			r <- headMaybe . filter (sameuuid t)
				<$> runThreadState st Remote.remoteList
			updateTransferInfo dstatus t info
				{ transferRemote = r }
		sameuuid t r = Remote.uuid r == transferUUID t

{- Called when a transfer information file is updated.
 -
 - The only thing that should change in the transfer info is the
 - bytesComplete, so that's the only thing updated in the DaemonStatus. -}
onModify :: Handler
onModify _ dstatus _ file _ = do
	case parseTransferFile file of
		Nothing -> noop
		Just t -> go t =<< readTransferInfoFile Nothing file
	where
		go _ Nothing = noop
		go t (Just newinfo) = alterTransferInfo t
			(\i -> i { bytesComplete = bytesComplete newinfo })
			dstatus

{- This thread can only watch transfer sizes when the DirWatcher supports
 - tracking modificatons to files. -}
watchesTransferSize :: Bool
watchesTransferSize = modifyTracked

{- Called when a transfer information file is removed. -}
onDel :: Handler
onDel st dstatus transferqueue file _ = case parseTransferFile file of
	Nothing -> noop
	Just t -> do
		brokendebug thisThread
			[ "transfer finishing:"
			, show t
			]
		minfo <- removeTransfer dstatus t

		void $ forkIO $ do
			{- XXX race workaround delay. The location
 			 - log needs to be updated before finishedTransfer
 			 - runs. -}
			threadDelay 10000000 -- 10 seconds
			finishedTransfer st dstatus transferqueue t minfo

{- Queue uploads of files we successfully downloaded, spreading them
 - out to other reachable remotes.
 -
 - Downloading a file may have caused a remote to not want it;
 - so drop it from the remote.
 -
 - Uploading a file may cause the local repo, or some other remote to not
 - want it; handle that too.
 -}
finishedTransfer :: ThreadState -> DaemonStatusHandle -> TransferQueue -> Transfer -> Maybe TransferInfo -> IO ()
finishedTransfer st dstatus transferqueue t (Just info)
	| transferDirection t == Download = runThreadState st $
		whenM (inAnnex $ transferKey t) $ do
			handleDrops dstatus False
				(transferKey t) (associatedFile info)
			queueTransfersMatching (/= transferUUID t)
				Later transferqueue dstatus
				(transferKey t) (associatedFile info) Upload
	| otherwise = runThreadState st $
		handleDrops dstatus True (transferKey t) (associatedFile info)
finishedTransfer _ _ _ _ _ = noop
