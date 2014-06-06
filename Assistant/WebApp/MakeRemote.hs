{- git-annex assistant webapp making remotes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.WebApp.MakeRemote (
	module Assistant.MakeRemote,
	module Assistant.WebApp.MakeRemote
) where

import Assistant.Common
import Assistant.WebApp.Types
import Assistant.Sync
import qualified Remote
import qualified Types.Remote as Remote
import qualified Config
import Config.Cost
import Types.StandardGroups
import Git.Types (RemoteName)
import Logs.PreferredContent
import Assistant.MakeRemote

import Utility.Yesod

{- Runs an action that creates or enables a cloud remote,
 - and finishes setting it up, then starts syncing with it,
 - and finishes by displaying the page to edit it.
 -
 - This includes displaying the connectionNeeded nudge if appropariate.
 -}
setupCloudRemote :: StandardGroup -> Maybe Cost -> Annex RemoteName -> Handler a
setupCloudRemote = setupRemote $ redirect . EditNewCloudRepositoryR . Remote.uuid

setupRemote :: (Remote -> Handler a) -> StandardGroup -> Maybe Cost -> Annex RemoteName -> Handler a
setupRemote postsetup defaultgroup mcost getname = do
	r <- liftAnnex $ addRemote getname
	liftAnnex $ do
		setStandardGroup (Remote.uuid r) defaultgroup
		maybe noop (Config.setRemoteCost (Remote.repo r)) mcost
	liftAssistant $ syncRemote r
	postsetup r
