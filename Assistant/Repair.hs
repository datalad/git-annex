{- git-annex assistant repository repair
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.Repair where

import Assistant.Common
import Command.Repair (repairAnnexBranch)
import Git.Repair (runRepairOf)
import Logs.FsckResults
import Annex.UUID
import Utility.Batch
import Config.Files
import Assistant.Sync

import Control.Concurrent.Async

runRepair :: UUID -> Assistant ()
runRepair u = do
	-- Stop the watcher from running while running repairs.
	changeSyncable Nothing False

	fsckresults <- liftAnnex $ readFsckResults u
	myu <- liftAnnex getUUID
	if u == myu
		then localrepair fsckresults
		else remoterepair fsckresults
	liftAnnex $ writeFsckResults u Nothing

	-- Start the watcher running again. This also triggers it to do a
	-- startup scan, which is especially important if the git repo
	-- repair removed files from the index file. Those files will be
	-- seen as new, and re-added to the repository.
	changeSyncable Nothing True
  where
  	localrepair fsckresults = do
		-- This intentionally runs the repair inside the Annex
		-- monad, which is not strictly necessary, but keeps
		-- other threads that might be trying to use the Annex
		-- from running until it completes.
		needfsck <- liftAnnex $ do
			(ok, stillmissing, modifiedbranches) <- inRepo $
				runRepairOf fsckresults True
			repairAnnexBranch stillmissing modifiedbranches
			return (not ok)
		when needfsck $
			backgroundfsck [ Param "--fast" ]

	remoterepair _fsckresults = do
		error "TODO: remote repair"
	
	backgroundfsck params = liftIO $ void $ async $ do
		program <- readProgramFile
		batchCommand program (Param "fsck" : params)
