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
myseek o = do
	-- Changes to top of repository, so when this is run in a
	-- subdirectory, it will still default to adding files anywhere in
	-- the working tree.
	Command.Sync.prepMerge

	Command.Add.seek Command.Add.AddOptions
		{ Command.Add.addThese = map fromOsPath $ 
			Command.Sync.contentOfOption o
		, Command.Add.batchOption = NoBatch
		, Command.Add.updateOnly = False
		, Command.Add.largeFilesOverride = Nothing
		, Command.Add.checkGitIgnoreOption = CheckGitIgnore True
		, Command.Add.dryRunOption = DryRun False
		}
	finishCommandActions
	-- Flush added files to index so they will be committed.
	Annex.Queue.flush

	Command.Sync.seek' o
