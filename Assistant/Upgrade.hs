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
import Utility.Url
import Utility.Env
import Types.Distribution

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
