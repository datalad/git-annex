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
import Annex.Content
import Logs.Transfer
import Utility.DirWatcher
import Utility.Types.DirWatcher
import qualified Remote

thisThread :: ThreadName
thisThread = "TransferWatcher"

{- This thread watches for changes to the gitAnnexTransferDir,
 - and updates the DaemonStatus's map of ongoing transfers. -}
transferWatcherThread :: ThreadState -> DaemonStatusHandle -> TransferQueue -> NamedThread
transferWatcherThread st dstatus transferqueue = thread $ do
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
	debug thisThread ["watching for transfers"]
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
			debug thisThread
				[ "transfer starting:"
				, show t
				]
			r <- headMaybe . filter (sameuuid t) . syncRemotes
				<$> getDaemonStatus dstatus
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
		go t (Just newinfo) = alterTransferInfo dstatus t $ \info ->
			info { bytesComplete = bytesComplete newinfo }

{- This thread can only watch transfer sizes when the DirWatcher supports
 - tracking modificatons to files. -}
watchesTransferSize :: Bool
watchesTransferSize = modifyTracked

{- Called when a transfer information file is removed. -}
onDel :: Handler
onDel st dstatus transferqueue file _ = case parseTransferFile file of
	Nothing -> noop
	Just t -> do
		debug thisThread
			[ "transfer finishing:"
			, show t
			]
		minfo <- removeTransfer dstatus t

		{- Queue uploads of files we successfully downloaded,
		 - spreading them out to other reachable remotes. -}
		case (minfo, transferDirection t) of
			(Just info, Download) -> runThreadState st $
				whenM (inAnnex $ transferKey t) $
					queueTransfersMatching 
						(/= transferUUID t)
						Later transferqueue dstatus
						(transferKey t)
						(associatedFile info)
						Upload
			_ -> noop
