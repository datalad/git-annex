{- git-annex command
 -
 - Copyright 2011, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Merge where

import Common.Annex
import Command
import qualified Annex.Branch
import qualified Git.Branch
import Command.Sync (prepMerge, mergeLocal)

cmd :: Command
cmd = command "merge" SectionMaintenance
	"automatically merge changes from remotes"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = do
	withNothing mergeBranch ps
	withNothing mergeSynced ps

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
	mergeLocal =<< inRepo Git.Branch.current
