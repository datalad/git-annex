{- git-annex command
 -
 - Copyright 2011, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Merge where

import Command
import qualified Annex.Branch
import Annex.CurrentBranch
import Command.Sync (prepMerge, mergeLocal, mergeConfig)

cmd :: Command
cmd = command "merge" SectionMaintenance
	"automatically merge changes from remotes"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek _ = do
	commandAction mergeBranch
	commandAction mergeSynced

mergeBranch :: CommandStart
mergeBranch = starting "merge" (ActionItemOther (Just "git-annex")) $ do
	Annex.Branch.update
	-- commit explicitly, in case no remote branches were merged
	Annex.Branch.commit =<< Annex.Branch.commitMessage
	next $ return True

mergeSynced :: CommandStart
mergeSynced = do
	prepMerge
	mergeLocal mergeConfig def =<< getCurrentBranch
