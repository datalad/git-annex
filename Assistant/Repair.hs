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
import Git.Fsck (FsckResults)
import Git.Repair (runRepairOf)
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
#endif

import qualified Data.Text as T
import Control.Concurrent.Async

{- Try to do a non-destructive repair. If that fails, pop up an alert. -}
brokenRepositoryDetected :: FsckResults -> UrlRenderer -> UUID -> Assistant ()
brokenRepositoryDetected fsckresults urlrenderer u = do
	liftAnnex $ writeFsckResults u fsckresults
	handle =<< runRepair u False
  where
	handle True = return ()
	handle False = do
#ifdef WITH_WEBAPP
		button <- mkAlertButton True (T.pack "Click Here") urlrenderer $
			RepairRepositoryR u
		void $ addAlert $ brokenRepositoryAlert button
#else
		return ()
#endif

runRepair :: UUID -> Bool -> Assistant Bool
runRepair u destructiverepair = do
	-- Stop the watcher from running while running repairs.
	changeSyncable Nothing False

	fsckresults <- liftAnnex $ readFsckResults u
	myu <- liftAnnex getUUID
	ok <- if u == myu
		then localrepair fsckresults
		else remoterepair fsckresults
	liftAnnex $ writeFsckResults u Nothing

	-- Start the watcher running again. This also triggers it to do a
	-- startup scan, which is especially important if the git repo
	-- repair removed files from the index file. Those files will be
	-- seen as new, and re-added to the repository.
	when ok $
		changeSyncable Nothing True

	return ok
  where
  	localrepair fsckresults = do
		-- This intentionally runs the repair inside the Annex
		-- monad, which is not strictly necessary, but keeps
		-- other threads that might be trying to use the Annex
		-- from running until it completes.
		ok <- liftAnnex $ do
			(ok, stillmissing, modifiedbranches) <- inRepo $
				runRepairOf fsckresults destructiverepair
			when destructiverepair $
				repairAnnexBranch stillmissing modifiedbranches
			return ok
		-- Run a background fast fsck if a destructive repair had
		-- to be done, to ensure that the git-annex branch
		-- reflects the current state of the repo.
		when (destructiverepair && not ok) $
			backgroundfsck [ Param "--fast" ]
		return ok

	remoterepair _fsckresults = do
		error "TODO: remote repair"
	
	backgroundfsck params = liftIO $ void $ async $ do
		program <- readProgramFile
		batchCommand program (Param "fsck" : params)
