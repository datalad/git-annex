{- git-annex assistant remote deletion utilities
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.DeleteRemote where

import Assistant.Common
import Assistant.Types.UrlRenderer
import Assistant.TransferQueue
import Types.Transfer
import Logs.Location
import Assistant.DaemonStatus
import qualified Remote
import Remote.List
import qualified Git.Remote.Remove
import Logs.Trust
import qualified Annex

#ifdef WITH_WEBAPP
import Assistant.WebApp.Types
import Assistant.Alert
import qualified Data.Text as T
#endif

{- Removes a remote (but leave the repository as-is), and returns the old
 - Remote data. -}
disableRemote :: UUID -> Assistant Remote
disableRemote uuid = do
	remote <- fromMaybe (error "unknown remote")
		<$> liftAnnex (Remote.remoteFromUUID uuid)
	liftAnnex $ do
		inRepo $ Git.Remote.Remove.remove (Remote.name remote)
		void $ remoteListRefresh
	updateSyncRemotes
	return remote

{- Removes a remote, marking it dead .-}
removeRemote :: UUID -> Assistant Remote
removeRemote uuid = do
	liftAnnex $ trustSet uuid DeadTrusted
	disableRemote uuid

{- Called when a Remote is probably empty, to remove it.
 -
 - This does one last check for any objects remaining in the Remote,
 - and if there are any, queues Downloads of them, and defers removing
 - the remote for later. This is to catch any objects not referred to
 - in keys in the current branch.
 -}
removableRemote :: UrlRenderer -> UUID -> Assistant ()
removableRemote urlrenderer uuid = do
	keys <- getkeys
	if null keys
		then finishRemovingRemote urlrenderer uuid
		else do
			r <- fromMaybe (error "unknown remote")
				<$> liftAnnex (Remote.remoteFromUUID uuid)
			mapM_ (queueremaining r) keys
  where
	queueremaining r k = 
		queueTransferWhenSmall "remaining object in unwanted remote"
			Nothing (Transfer Download uuid k) r
	{- Scanning for keys can take a long time; do not tie up
	 - the Annex monad while doing it, so other threads continue to
	 - run. -}
	getkeys = do
		a <- liftAnnex $ Annex.withCurrentState $ loggedKeysFor uuid
		liftIO a

{- With the webapp, this asks the user to click on a button to finish
 - removing the remote.
 -
 - Without the webapp, just do the removal now.
 -}
finishRemovingRemote :: UrlRenderer -> UUID -> Assistant ()
#ifdef WITH_WEBAPP
finishRemovingRemote urlrenderer uuid = do
	desc <- liftAnnex $ Remote.prettyUUID uuid
	button <- mkAlertButton True (T.pack "Finish deletion process") urlrenderer $
		FinishDeleteRepositoryR uuid
	void $ addAlert $ remoteRemovalAlert desc button
#else
finishRemovingRemote _ uuid = void $ removeRemote uuid
#endif
