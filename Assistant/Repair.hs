{- git-annex assistant repository repair
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Repair where

import Assistant.Common
import Command.Repair (repairAnnexBranch)
import Git.Fsck (FsckResults, foundBroken)
import Git.Repair (runRepairOf)
import qualified Git
import qualified Remote
import qualified Types.Remote as Remote
import Logs.FsckResults
import Annex.UUID
import Utility.Batch
import Config.Files
import Assistant.Sync
import Assistant.Alert
import Assistant.DaemonStatus
import Assistant.Types.UrlRenderer
#ifdef WITH_WEBAPP
import Assistant.WebApp.Types
import qualified Data.Text as T
#endif

import Control.Concurrent.Async

{- When the FsckResults require a repair, tries to do a non-destructive
 - repair. If that fails, pops up an alert. -}
repairWhenNecessary :: UrlRenderer -> UUID -> Maybe Remote -> FsckResults -> Assistant Bool
repairWhenNecessary urlrenderer u mrmt fsckresults
	| foundBroken fsckresults = do
		liftAnnex $ writeFsckResults u fsckresults
		repodesc <- liftAnnex $ Remote.prettyUUID u
		ok <- alertWhile (repairingAlert repodesc)
			(runRepair u mrmt False)
#ifdef WITH_WEBAPP
		unless ok $ do
			button <- mkAlertButton True (T.pack "Click Here") urlrenderer $
				RepairRepositoryR u
			void $ addAlert $ brokenRepositoryAlert button
#endif
		return ok
	| otherwise = return False

runRepair :: UUID -> Maybe Remote -> Bool -> Assistant Bool
runRepair u mrmt destructiverepair = do
	fsckresults <- liftAnnex $ readFsckResults u
	myu <- liftAnnex getUUID
	ok <- if u == myu
		then localrepair fsckresults
		else remoterepair fsckresults
	liftAnnex $ writeFsckResults u Nothing
	debug [ "Repaired", show u, show ok ]

	return ok
  where
  	localrepair fsckresults = do
		-- Stop the watcher from running while running repairs.
		changeSyncable Nothing False

		-- This intentionally runs the repair inside the Annex
		-- monad, which is not strictly necessary, but keeps
		-- other threads that might be trying to use the Annex
		-- from running until it completes.
		ok <- liftAnnex $ repair fsckresults Nothing

		-- Run a background fast fsck if a destructive repair had
		-- to be done, to ensure that the git-annex branch
		-- reflects the current state of the repo.
		when destructiverepair $
			backgroundfsck [ Param "--fast" ]

		-- Start the watcher running again. This also triggers it to
		-- do a startup scan, which is especially important if the
		-- git repo repair removed files from the index file. Those
		-- files will be seen as new, and re-added to the repository.
		when (ok || destructiverepair) $
			changeSyncable Nothing True

		return ok

	remoterepair fsckresults = case Remote.repairRepo =<< mrmt of
		Nothing -> return False
		Just mkrepair -> do
			thisrepopath <- liftIO . absPath
				=<< liftAnnex (fromRepo Git.repoPath)
			a <- liftAnnex $ mkrepair $
				repair fsckresults (Just thisrepopath)
			liftIO $ catchBoolIO a

	repair fsckresults referencerepo = do
		(ok, stillmissing, modifiedbranches) <- inRepo $
			runRepairOf fsckresults destructiverepair referencerepo
		when destructiverepair $
			repairAnnexBranch stillmissing modifiedbranches
		return ok
	
	backgroundfsck params = liftIO $ void $ async $ do
		program <- readProgramFile
		batchCommand program (Param "fsck" : params)
