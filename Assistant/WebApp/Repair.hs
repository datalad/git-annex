{- git-annex assistant repository repair
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Repair where

import Assistant.WebApp.Common
import Assistant.WebApp.RepoList
import Remote (prettyUUID, remoteFromUUID)
import Annex.UUID (getUUID)
import Assistant.Repair

getRepairRepositoryR :: UUID -> Handler Html
getRepairRepositoryR = postRepairRepositoryR
postRepairRepositoryR :: UUID -> Handler Html
postRepairRepositoryR u = page "Repair repository" Nothing $ do
	repodesc <- liftAnnex $ prettyUUID u
	repairingmainrepo <- (==) u <$> liftAnnex getUUID
	$(widgetFile "control/repairrepository")

getRepairRepositoryRunR :: UUID -> Handler Html
getRepairRepositoryRunR = postRepairRepositoryRunR
postRepairRepositoryRunR :: UUID -> Handler Html
postRepairRepositoryRunR u = do
	r <- liftAnnex $ remoteFromUUID u
	void $ liftAssistant $ runRepair u r True
	page "Repair repository" Nothing $ do
		let repolist = repoListDisplay $
			mainRepoSelector { nudgeAddMore = True }
		$(widgetFile "control/repairrepository/done")
