{- git-annex command
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Merge where

import Command
import qualified Annex.Branch
import qualified Git
import qualified Git.Branch
import Annex.CurrentBranch
import Command.Sync (prepMerge, mergeLocal, mergeConfig, merge)

cmd :: Command
cmd = command "merge" SectionMaintenance
	"merge changes from remotes"
	(paramOptional paramRef) (withParams seek)

seek :: CmdParams -> CommandSeek
seek [] = do
	prepMerge
	commandAction mergeAnnexBranch
	commandAction mergeSyncedBranch
seek bs = do
	prepMerge
	forM_ bs (commandAction . mergeBranch . Git.Ref)

mergeAnnexBranch :: CommandStart
mergeAnnexBranch = starting "merge" (ActionItemOther (Just "git-annex")) $ do
	Annex.Branch.update
	-- commit explicitly, in case no remote branches were merged
	Annex.Branch.commit =<< Annex.Branch.commitMessage
	next $ return True

mergeSyncedBranch :: CommandStart
mergeSyncedBranch = mergeLocal mergeConfig def =<< getCurrentBranch

mergeBranch :: Git.Ref -> CommandStart
mergeBranch r = starting "merge" (ActionItemOther (Just (Git.fromRef r))) $ do
	currbranch <- getCurrentBranch
	next $ merge currbranch mergeConfig def Git.Branch.ManualCommit r
