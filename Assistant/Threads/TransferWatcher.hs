{- git-annex assistant transfer watching thread
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.TransferWatcher where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.TransferSlots
import Types.Transfer
import Logs.Transfer
import Utility.DirWatcher
import Utility.DirWatcher.Types
import qualified Remote

import Control.Concurrent
import qualified Data.Map as M

{- This thread watches for changes to the gitAnnexTransferDir,
 - and updates the DaemonStatus's map of ongoing transfers. -}
transferWatcherThread :: NamedThread
transferWatcherThread = namedThread "TransferWatcher" $ do
	dir <- liftAnnex $ gitAnnexTransferDir <$> gitRepo
	liftIO $ createDirectoryIfMissing True dir
	let hook a = Just <$> asIO2 (runHandler a)
	addhook <- hook onAdd
	delhook <- hook onDel
	modifyhook <- hook onModify
	errhook <- hook onErr
	let hooks = mkWatchHooks
		{ addHook = addhook
		, delHook = delhook
		, modifyHook = modifyhook
		, errHook = errhook
		}
	void $ liftIO $ watchDir dir (const False) True hooks id
	debug ["watching for transfers"]

type Handler = FilePath -> Assistant ()

{- Runs an action handler.
 -
 - Exceptions are ignored, otherwise a whole thread could be crashed.
 -}
runHandler :: Handler -> FilePath -> Maybe FileStatus -> Assistant ()
runHandler handler file _filestatus =
	either (liftIO . print) (const noop) =<< tryIO <~> handler file

{- Called when there's an error with inotify. -}
onErr :: Handler
onErr = error

{- Called when a new transfer information file is written. -}
onAdd :: Handler
onAdd file = case parseTransferFile file of
	Nothing -> noop
	Just t -> go t =<< liftAnnex (checkTransfer t)
  where
	go _ Nothing = noop -- transfer already finished
	go t (Just info) = do
		debug [ "transfer starting:", describeTransfer t info ]
		r <- liftAnnex $ Remote.remoteFromUUID $ transferUUID t
		updateTransferInfo t info { transferRemote = r }

{- Called when a transfer information file is updated.
 -
 - The only thing that should change in the transfer info is the
 - bytesComplete, so that's the only thing updated in the DaemonStatus. -}
onModify :: Handler
onModify file = case parseTransferFile file of
	Nothing -> noop
	Just t -> go t =<< liftIO (readTransferInfoFile Nothing file)
  where
	go _ Nothing = noop
	go t (Just newinfo) = alterTransferInfo t $
		\i -> i { bytesComplete = bytesComplete newinfo }

{- This thread can only watch transfer sizes when the DirWatcher supports
 - tracking modificatons to files. -}
watchesTransferSize :: Bool
watchesTransferSize = modifyTracked

{- Called when a transfer information file is removed. -}
onDel :: Handler
onDel file = case parseTransferFile file of
	Nothing -> noop
	Just t -> do
		debug [ "transfer finishing:", show t]
		minfo <- removeTransfer t

		-- Run transfer hook.
		m <- transferHook <$> getDaemonStatus
		maybe noop (\hook -> void $ liftIO $ forkIO $ hook t)
			(M.lookup (transferKey t) m)

		finished <- asIO2 finishedTransfer
		void $ liftIO $ forkIO $ do
			{- XXX race workaround delay. The location
			 - log needs to be updated before finishedTransfer
			 - runs. -}
			threadDelay 10000000 -- 10 seconds
			finished t minfo
