{- git-annex assistant export updating thread
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Threads.Exporter where

import Assistant.Common
import Assistant.Commits
import Assistant.Pushes
import Assistant.DaemonStatus
import Annex.Concurrent
import Annex.CurrentBranch
import Utility.ThreadScheduler
import qualified Annex
import qualified Remote
import qualified Types.Remote as Remote
import qualified Command.Sync

import Control.Concurrent.Async
import Data.Time.Clock
import qualified Data.Map as M

{- This thread retries exports that failed before. -}
exportRetryThread :: NamedThread
exportRetryThread = namedThread "ExportRetrier" $ runEvery (Seconds halfhour) <~> do
	-- We already waited half an hour, now wait until there are failed
	-- exports to retry.
	toexport <- getFailedPushesBefore (fromIntegral halfhour) 
		=<< getAssistant failedExportMap
	unless (null toexport) $ do
		debug ["retrying", show (length toexport), "failed exports"]
		void $ exportToRemotes toexport
  where
	halfhour = 1800

{- This thread updates exports soon after git commits are made. -}
exportThread :: NamedThread
exportThread = namedThread "Exporter" $ runEvery (Seconds 30) <~> do
	-- We already waited two seconds as a simple rate limiter.
	-- Next, wait until at least one commit has been made
	void getExportCommits
	-- Now see if now's a good time to push.
	void $ exportToRemotes =<< exportTargets

{- We want to avoid exporting to remotes that are marked readonly.
 -
 - Also, avoid exporting to local remotes we can easily tell are not available,
 - to avoid ugly messages when a removable drive is not attached.
 -}
exportTargets :: Assistant [Remote]
exportTargets = liftAnnex . filterM (Remote.checkAvailable True)
	=<< candidates <$> getDaemonStatus
  where
	candidates = filter (not . Remote.readonly) . exportRemotes

exportToRemotes :: [Remote] -> Assistant ()
exportToRemotes rs = do
	-- This is a long-duration action which runs in the Annex monad,
	-- so don't just liftAnnex to run it; fork the Annex state.
	runner <- liftAnnex $ forkState $
		forM rs $ \r -> do
			Annex.changeState $ \st -> st { Annex.errcounter = 0 }
			start <- liftIO getCurrentTime
			void $ Command.Sync.seekExportContent Nothing rs
				=<< getCurrentBranch
			-- Look at command error counter to see if the export
			-- didn't work.
			failed <- (> 0) <$> Annex.getState Annex.errcounter
			Annex.changeState $ \st -> st { Annex.errcounter = 0 }
			return $ if failed
				then Just (r, start)
				else Nothing
	failed <- catMaybes
		<$> (liftAnnex =<< liftIO . wait =<< liftIO (async runner))
	unless (null failed) $ do
		v <- getAssistant failedExportMap
		changeFailedPushMap v $ M.union $ M.fromList failed
