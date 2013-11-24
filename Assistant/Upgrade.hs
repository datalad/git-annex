{- git-annex assistant upgrading
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Upgrade where

import Assistant.Common
import Assistant.Restart
import qualified Annex
import Assistant.Alert
import Assistant.DaemonStatus
import Utility.Env
import Types.Distribution
import Logs.Transfer
import Logs.Web
import Annex.Content
import qualified Backend
import qualified Types.Backend
import qualified Types.Key

{- Upgrade without interaction in the webapp. -}
unattendedUpgrade :: Assistant ()
unattendedUpgrade = do
	prepUpgrade
	url <- runRestart
	postUpgrade url

prepUpgrade :: Assistant ()
prepUpgrade = do
	void $ addAlert upgradingAlert
	void $ liftIO $ setEnv upgradedEnv "1" True
	prepRestart

postUpgrade :: URLString -> Assistant ()
postUpgrade = postRestart

autoUpgradeEnabled :: Assistant Bool
autoUpgradeEnabled = liftAnnex $ (==) AutoUpgrade . annexAutoUpgrade <$> Annex.getGitConfig

checkSuccessfulUpgrade :: IO Bool
checkSuccessfulUpgrade = isJust <$> getEnv upgradedEnv

upgradedEnv :: String
upgradedEnv = "GIT_ANNEX_UPGRADED"

{- Fsck the key to verify the download. -}
distributionDownloadComplete :: GitAnnexDistribution -> Transfer -> Assistant ()
distributionDownloadComplete d t 
	| transferDirection t == Download = do
		maybe noop upgradeToDistribution
			=<< liftAnnex (withObjectLoc k fsckit (getM fsckit))
		liftAnnex $ setUrlMissing k (distributionUrl d)
	| otherwise = noop
  where
	k = distributionKey d
	fsckit f = case Backend.maybeLookupBackendName (Types.Key.keyBackendName k) of
		Nothing -> return $ Just f
		Just b -> case Types.Backend.fsckKey b of
			Nothing -> return $ Just f
			Just a -> ifM (a k f)
				( return $ Just f
				, do
					-- unlikely to resume a bad
					-- download from web
					liftIO $ nukeFile f
					return Nothing
				)

upgradeToDistribution :: FilePath -> Assistant ()
upgradeToDistribution f = error "TODO"
