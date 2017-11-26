{- git-annex assistant thread to detect when upgrade is available
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Threads.Upgrader (
	upgraderThread
) where

import Assistant.Common
import Assistant.Upgrade

import Assistant.Types.UrlRenderer
import Assistant.DaemonStatus
import Assistant.Alert
import Utility.NotificationBroadcaster
import qualified Annex
import qualified Build.SysConfig
import qualified Utility.DottedVersion as DottedVersion
import Types.Distribution
#ifdef WITH_WEBAPP
import Assistant.WebApp.Types
#endif

import Data.Time.Clock
import qualified Data.Text as T

upgraderThread :: UrlRenderer -> NamedThread
upgraderThread urlrenderer = namedThread "Upgrader" $
	when (isJust Build.SysConfig.upgradelocation) $ do
		{- Check for upgrade on startup, unless it was just
		 - upgraded. -}
		unlessM (liftIO checkSuccessfulUpgrade) $
			checkUpgrade urlrenderer
		h <- liftIO . newNotificationHandle False . networkConnectedNotifier =<< getDaemonStatus
		go h =<< liftIO getCurrentTime
  where
	{- Wait for a network connection event. Then see if it's been
	 - half a day since the last upgrade check. If so, proceed with
	 - check. -}
	go h lastchecked = do
		liftIO $ waitNotification h
		autoupgrade <- liftAnnex $ annexAutoUpgrade <$> Annex.getGitConfig
		if autoupgrade == NoAutoUpgrade
			then go h lastchecked
			else do
				now <- liftIO getCurrentTime
				if diffUTCTime now lastchecked > halfday
					then do
						checkUpgrade urlrenderer
						go h =<< liftIO getCurrentTime
					else go h lastchecked
	halfday = 12 * 60 * 60

checkUpgrade :: UrlRenderer -> Assistant ()
checkUpgrade urlrenderer = do
	debug [ "Checking if an upgrade is available." ]
	go =<< downloadDistributionInfo
  where
	go Nothing = debug [ "Failed to check if upgrade is available." ]
	go (Just d) = do
		let installed = DottedVersion.normalize Build.SysConfig.packageversion
		let avail = DottedVersion.normalize $ distributionVersion d
		let old = DottedVersion.normalize <$> distributionUrgentUpgrade d
		if Just installed <= old
			then canUpgrade High urlrenderer d
			else if installed < avail
				then canUpgrade Low urlrenderer d
				else debug [ "No new version found." ]

canUpgrade :: AlertPriority -> UrlRenderer -> GitAnnexDistribution -> Assistant ()
canUpgrade urgency urlrenderer d = ifM autoUpgradeEnabled
	( startDistributionDownload d
	, do
#ifdef WITH_WEBAPP
		button <- mkAlertButton True (T.pack "Upgrade") urlrenderer (ConfigStartUpgradeR d)
		void $ addAlert (canUpgradeAlert urgency (distributionVersion d) button)
#else
		noop
#endif
	)
