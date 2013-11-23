{- git-annex assistant webapp upgrade UI
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.Upgrade where

import Assistant.WebApp.Common
import qualified Annex
import Types.Distribution
import Assistant.WebApp.OtherRepos
import Assistant.Threads.Watcher
import Assistant.Alert
import Assistant.DaemonStatus
import Assistant.NamedThread
import Utility.ThreadScheduler
import Utility.HumanTime
import Git

import Data.Time.Clock
import Control.Concurrent
import System.Posix (getProcessID, signalProcess, sigTERM)

getConfigStartUpgradeR :: GitAnnexDistribution -> Handler Html
getConfigStartUpgradeR d = page "Upgrade git-annex" (Just Configuration) $ do
	now <- liftIO getCurrentTime
	let upgradeage = Duration $ floor $
		now `diffUTCTime` distributionReleasedate d
	$(widgetFile "configurators/upgrade/start")

{- Finish upgrade by starting the new assistant in the same repository this
 - one is running in, and redirecting to it.
 -
 - Before the new assistant can be started, have to remove our 
 - gitAnnexUrlFile and our gitAnnexPidFile. Pausing the watcher is also
 - a good idea, to avoid fighting when two assistants are running in the
 - same repo.
 -
 - Note that only the tag that requested this page gets redirected.
 - If the user has multiple web browser tabs open to the webapp,
 - the others will show the upgradingAlert, and keep running until
 - this process is terminated.
 -}
getConfigFinishUpgradeR :: Handler Html
getConfigFinishUpgradeR = do
	liftAssistant $ void $ addAlert upgradingAlert
	liftIO . maybe noop (`throwTo` PauseWatcher) =<< liftAssistant (namedThreadId watchThread)
	liftIO . nukeFile =<< liftAnnex (fromRepo gitAnnexUrlFile)
	liftIO . nukeFile =<< liftAnnex (fromRepo gitAnnexPidFile)
	reapself `after` startnewprocess
  where
	-- Wait for the redirect to be served to the browser
	-- before terminating this process.
	reapself = liftIO $ void $ forkIO $ do
		threadDelaySeconds (Seconds 120)
		signalProcess sigTERM =<< getProcessID
	startnewprocess = switchToAssistant
		=<< liftAnnex (repoLocation <$> Annex.gitRepo)
