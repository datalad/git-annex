{- git-annex assistant remote deletion utilities
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.DeleteRemote where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.Alert
import Assistant.DaemonStatus
import qualified Remote
import Remote.List
import qualified Git.Command
import Logs.Trust

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

{- Called when a remote was marked as unwanted, and is now empty, so can be
 - removed. -}
finishRemovingRemote :: UrlRenderer -> UUID -> Assistant ()
finishRemovingRemote urlrenderer uuid = do
	void $ removeRemote uuid
	liftAnnex $ trustSet uuid DeadTrusted

	desc <- liftAnnex $ Remote.prettyUUID uuid
	url <- liftIO $ renderUrl urlrenderer (FinishedDeletingRepositoryContentsR uuid) []
	close <- asIO1 removeAlert
	void $ addAlert $ remoteRemovalAlert desc $ AlertButton
		{ buttonLabel = T.pack "Finish removal"
		, buttonUrl = url
		, buttonAction = Just close
		}
