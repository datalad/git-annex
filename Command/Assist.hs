{- git-annex command
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Assist (cmd) where

import Command
import qualified Command.Sync
import qualified Command.Add
import Annex.CheckIgnore
import qualified Annex.Queue

cmd :: Command
cmd = withAnnexOptions [jobsOption, backendOption] $
	command "assist" SectionCommon 
		"add files and sync changes with remotes"
		(paramRepeating paramRemote)
		(myseek <--< Command.Sync.optParser Command.Sync.AssistMode)

myseek :: Command.Sync.SyncOptions -> CommandSeek
myseek o = startConcurrency transferStages $ do
	-- Run before prepMerge so it adds only files in the current
	-- directory and below, not new files elsewhere in the working
	-- tree.
	Command.Add.seek Command.Add.AddOptions
		{ Command.Add.addThese = []
		, Command.Add.batchOption = NoBatch
		, Command.Add.updateOnly = False
		, Command.Add.largeFilesOverride = Nothing
		, Command.Add.checkGitIgnoreOption = CheckGitIgnore (False)
		, Command.Add.dryRunOption = DryRun False
		}
	waitForAllRunningCommandActions
	-- Flush added files to index so they will be committed.
	Annex.Queue.flush

	Command.Sync.prepMerge
	Command.Sync.seek' o
