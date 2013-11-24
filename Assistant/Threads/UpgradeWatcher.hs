{- git-annex assistant thread to detect when git-annex is upgraded
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Threads.UpgradeWatcher (
	upgradWatcherThread
) where

import Assistant.Common
import Assistant.Upgrade
import Utility.DirWatcher
import Utility.DirWatcher.Types
import Utility.ThreadScheduler
import Assistant.Types.UrlRenderer
import Assistant.Alert
import Assistant.DaemonStatus
#ifdef WITH_WEBAPP
import Assistant.WebApp.Types
import qualified Build.SysConfig
#endif

import Control.Concurrent.MVar
import qualified Data.Text as T

data WatcherState = InStartupScan | Started | Upgrading
	deriving (Eq)

upgradWatcherThread :: UrlRenderer -> NamedThread
upgradWatcherThread urlrenderer = namedThread "UpgradeWatcher" $ do
	whenM (liftIO $ checkSuccessfulUpgrade) $
		showSuccessfulUpgrade urlrenderer
	go =<< liftIO upgradeFlagFile
  where
	go Nothing = debug [ "cannot determine program path" ]
	go (Just flagfile) = do
		mvar <- liftIO $ newMVar InStartupScan
		changed <- Just <$> asIO2 (changedFile urlrenderer mvar flagfile)
		let hooks = mkWatchHooks
			{ addHook = changed
			, delHook = changed
			, addSymlinkHook = changed
			, modifyHook = changed
			, delDirHook = changed
			}
		let dir = parentDir flagfile
		let depth = length (splitPath dir) + 1
		let nosubdirs f = length (splitPath f) == depth
		void $ liftIO $ watchDir dir nosubdirs hooks (startup mvar)
  	-- Ignore bogus events generated during the startup scan.
  	startup mvar scanner = do
		r <- scanner
		void $ swapMVar mvar Started
		return r

changedFile :: UrlRenderer -> MVar WatcherState -> FilePath -> FilePath -> Maybe FileStatus -> Assistant ()
changedFile urlrenderer mvar flagfile file _status
	| flagfile /= file = noop
	| otherwise = do
		state <- liftIO $ readMVar mvar
		when (state == Started) $ do
			setstate Upgrading
			ifM (liftIO upgradeSanityCheck)
				( handleUpgrade urlrenderer
				, do
					debug ["new version failed sanity check; not using"]
					setstate Started
				)
  where
	setstate = void . liftIO . swapMVar mvar

handleUpgrade :: UrlRenderer -> Assistant ()
handleUpgrade urlrenderer = do
	-- Wait 2 minutes for any final upgrade changes to settle.
	-- (For example, other associated files may be being put into
	-- place.)
	liftIO $ threadDelaySeconds (Seconds 120)
	ifM autoUpgradeEnabled
		( do
			debug ["starting automatic upgrade"]
			unattendedUpgrade
#ifdef WITH_WEBAPP
		, do
			button <- mkAlertButton True (T.pack "Finish Upgrade") urlrenderer ConfigFinishUpgradeR
			void $ addAlert $ upgradeReadyAlert button
#else
		, noop
#endif
		)

showSuccessfulUpgrade :: UrlRenderer -> Assistant ()
showSuccessfulUpgrade urlrenderer = do
#ifdef WITH_WEBAPP
	button <- ifM autoUpgradeEnabled 
		( pure Nothing
		, Just <$> mkAlertButton True
			(T.pack "Enable Automatic Upgrades")
			urlrenderer ConfigEnableAutomaticUpgradeR
		)
	void $ addAlert $ upgradeFinishedAlert button Build.SysConfig.packageversion
#else
	noop
#endif
