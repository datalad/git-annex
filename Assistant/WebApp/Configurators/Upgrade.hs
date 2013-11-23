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
import Assistant.DaemonStatus
import Config
import Assistant.TransferQueue
import Assistant.TransferSlots
import Logs.Transfer
import Logs.Web
import Remote

import qualified Data.Map as M

{- On Android, just redirect the user's web browser to the apk file
 - to download it.
 -
 - Otherwise, register a hook action that will be called once the key
 - is downloaded, and start downloading the key.
 - -}
getConfigStartUpgradeR :: GitAnnexDistribution -> Handler Html
getConfigStartUpgradeR d = do
#ifdef __ANDROID__
	redirect (distributionUrl d)
#else
	liftAssistant $ do
		let k = distributionKey d
		let u = distributionUrl d
		liftAnnex $ setUrlPresent k u
		hook <- asIO1 $ downloadComplete d
		modifyDaemonStatus_ $ \status -> status
			{ transferHook = M.insert k hook (transferHook status) }
		let t = Transfer
			{ transferDirection = Download
			, transferUUID = webUUID
			, transferKey = k
			}
		let f = takeFileName u ++ " (for upgrade)"
		maybe noop (queueTransfer "upgrade" Next (Just f) t)
			=<< liftAnnex (remoteFromUUID webUUID)
		startTransfer t
	redirect DashboardR
#endif

downloadComplete :: GitAnnexDistribution -> Transfer -> Assistant ()
downloadComplete d t = do
	error "TODO"
	liftAnnex $ setUrlMissing (distributionKey d) (distributionUrl d)

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
