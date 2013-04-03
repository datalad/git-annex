{- git-annex assistant remote deletion utilities
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.DeleteRemote where

import Assistant.Common
#ifdef WITH_WEBAPP
import Assistant.WebApp.Types
import Assistant.WebApp
#endif
import Assistant.TransferQueue
import Logs.Transfer
import Logs.Location
import Assistant.Alert
import Assistant.DaemonStatus
import Assistant.Types.UrlRenderer
import qualified Remote
import Remote.List
import qualified Git.Command
import Logs.Trust
import qualified Annex

import qualified Data.Text as T

{- Removes a remote (but leave the repository as-is), and returns the old
 - Remote data. -}
removeRemote :: UUID -> Assistant Remote
removeRemote uuid = do
	remote <- fromMaybe (error "unknown remote")
		<$> liftAnnex (Remote.remoteFromUUID uuid)
	liftAnnex $ do
		inRepo $ Git.Command.run
			[ Param "remote"
			, Param "remove"
			, Param (Remote.name remote)
			]
		void $ remoteListRefresh
	updateSyncRemotes
	return remote

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

finishRemovingRemote :: UrlRenderer -> UUID -> Assistant ()
finishRemovingRemote urlrenderer uuid = do
	void $ removeRemote uuid
	liftAnnex $ trustSet uuid DeadTrusted

#ifdef WITH_WEBAPP
	desc <- liftAnnex $ Remote.prettyUUID uuid
	url <- liftIO $ renderUrl urlrenderer (FinishedDeletingRepositoryContentsR uuid) []
	close <- asIO1 removeAlert
	void $ addAlert $ remoteRemovalAlert desc $ AlertButton
		{ buttonLabel = T.pack "Finish removal"
		, buttonUrl = url
		, buttonAction = Just close
		}
#endif
