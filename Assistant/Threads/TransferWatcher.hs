{- git-annex assistant transfer watching thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.TransferWatcher where

import Common.Annex
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Logs.Transfer
import Utility.DirWatcher
import Utility.Types.DirWatcher
import Annex.BranchState

import Data.Map as M

{- This thread watches for changes to the gitAnnexTransferDir,
 - and updates the DaemonStatus's map of ongoing transfers. -}
transferWatcherThread :: ThreadState -> DaemonStatusHandle -> IO ()
transferWatcherThread st dstatus = do
	g <- runThreadState st $ fromRepo id
	let dir = gitAnnexTransferDir g
	createDirectoryIfMissing True dir
	let hook a = Just $ runHandler st dstatus a
	let hooks = mkWatchHooks
		{ addHook = hook onAdd
		, delHook = hook onDel
		, errHook = hook onErr
		}
	void $ watchDir dir (const False) hooks id

type Handler = ThreadState -> DaemonStatusHandle -> FilePath -> Maybe FileStatus -> IO ()

{- Runs an action handler.
 -
 - Exceptions are ignored, otherwise a whole thread could be crashed.
 -}
runHandler :: ThreadState -> DaemonStatusHandle -> Handler -> FilePath -> Maybe FileStatus -> IO ()
runHandler st dstatus handler file filestatus = void $ do
        either print (const noop) =<< tryIO go
        where
                go = handler st dstatus file filestatus

{- Called when there's an error with inotify. -}
onErr :: Handler
onErr _ _ msg _ = error msg

{- Called when a new transfer information file is written. -}
onAdd :: Handler
onAdd st dstatus file _ = case parseTransferFile file of
	Nothing -> noop
	Just t -> runThreadState st $ go t =<< checkTransfer t
	where
		go _ Nothing = noop -- transfer already finished
		go t (Just info) = adjustTransfers dstatus $
			M.insertWith' const t info

{- Called when a transfer information file is removed.
 -
 - When the transfer process is a child of this process, wait on it
 - to avoid zombies.
 -}
onDel :: Handler
onDel st dstatus file _ = case parseTransferFile file of
	Nothing -> noop
	Just t -> maybe noop waitchild
		=<< runThreadState st (removeTransfer dstatus t)
	where
		waitchild info
			| shouldWait info = case transferPid info of
				Nothing -> noop
				Just pid -> do
					void $ getProcessStatus True False pid
					runThreadState st invalidateCache
			| otherwise = noop	
