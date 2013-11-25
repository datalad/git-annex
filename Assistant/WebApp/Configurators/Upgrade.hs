{- git-annex assistant webapp upgrade UI
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.Upgrade where

import Assistant.WebApp.Common
import Types.Distribution
import Assistant.Upgrade
import Assistant.Restart
import Config

{- On Android, just point the user at the apk file to download.
 - Installation will be handled by selecting the downloaded file.
 -
 - Otherwise, start the upgrade process, which will run fully
 - noninteractively.
 - -}
getConfigStartUpgradeR :: GitAnnexDistribution -> Handler Html
getConfigStartUpgradeR d = do
#ifdef ANDROID_SPLICES
	let url = distributionUrl d
	page "Upgrade" (Just Configuration) $
		$(widgetFile "configurators/upgrade/android")
#else
	liftAssistant $ startDistributionDownload d
	redirect DashboardR
#endif

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
