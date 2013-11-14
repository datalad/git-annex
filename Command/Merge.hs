{- git-annex command
 -
 - Copyright 2011, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Merge where

import Common.Annex
import Command
import qualified Annex.Branch
import qualified Git.Branch
import Command.Sync (prepMerge, mergeLocal)

def :: [Command]
def = [command "merge" paramNothing seek SectionMaintenance
	"automatically merge changes from remotes"]

seek :: [CommandSeek]
seek =
	[ withNothing mergeBranch
	, withNothing mergeSynced
	]

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
