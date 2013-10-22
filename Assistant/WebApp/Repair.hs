{- git-annex assistant repository repair
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Repair where

import Assistant.WebApp.Common
import Remote (prettyUUID)

getRepairRepositoryR :: UUID -> Handler Html
getRepairRepositoryR = postRepairRepositoryR
postRepairRepositoryR :: UUID -> Handler Html
postRepairRepositoryR u = page "Repair repository" Nothing $ do
	repodesc <- liftAnnex $ prettyUUID u
	$(widgetFile "control/repairrepository")

getRepairRepositoryRunR :: UUID -> Handler Html
getRepairRepositoryRunR = postRepairRepositoryR
postRepairRepositoryRunR :: UUID -> Handler Html
postRepairRepositoryRunR u = page "Repair repository" Nothing $ do
	$(widgetFile "control/repairrepository/run")
