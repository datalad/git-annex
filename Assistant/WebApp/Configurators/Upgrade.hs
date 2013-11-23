{- git-annex assistant webapp upgrade UI
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.Upgrade where

import Assistant.WebApp.Common
import Types.Distribution
import Assistant.Upgrade
import Assistant.Restart
import Utility.HumanTime
import Config

import Data.Time.Clock

getConfigStartUpgradeR :: GitAnnexDistribution -> Handler Html
getConfigStartUpgradeR d = page "Upgrade git-annex" (Just Configuration) $ do
	now <- liftIO getCurrentTime
	let upgradeage = Duration $ floor $
		now `diffUTCTime` distributionReleasedate d
	$(widgetFile "configurators/upgrade/start")

{- Finish upgrade by starting the new assistant in the same repository this
 - one is running in, and redirecting to it. -}
getConfigFinishUpgradeR :: Handler Html
getConfigFinishUpgradeR = do
	liftAssistant prepUpgrade
	url <- liftAssistant runRestart
	liftAssistant $ postUpgrade url
	redirect url

getConfigEnableAutomaticUpgradeR :: Handler Html
getConfigEnableAutomaticUpgradeR = do
	liftAnnex $ setConfig (annexConfig "autoupgrade")
		(fromAutoUpgrade AutoUpgrade)
	redirect DashboardR
