{- git-annex command
 -
 - Copyright 2011, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Merge where

import Command
import qualified Annex.Branch
import Command.Sync (prepMerge, mergeLocal, getCurrBranch, mergeConfig)

cmd :: Command
cmd = command "merge" SectionMaintenance
	"automatically merge changes from remotes"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek _ = do
	commandAction mergeBranch
	commandAction mergeSynced

mergeBranch :: CommandStart
mergeBranch = do
	showStart "merge" "git-annex"
	next $ do
		Annex.Branch.update
		-- commit explicitly, in case no remote branches were merged
		Annex.Branch.commit "update"
		next $ return True

mergeSynced :: CommandStart
mergeSynced = do
	prepMerge
	mergeLocal mergeConfig =<< join getCurrBranch
